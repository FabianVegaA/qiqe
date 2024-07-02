use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::Mutex;

use actix_cors::Cors;
use actix_files::NamedFile;
use actix_web::{middleware::Logger, web, App, HttpRequest, HttpResponse, HttpServer};
use actix_web::http::header::ContentType;
use actix_web::http::StatusCode;
use awc;
use chrono::Utc;
use env_logger;
use log::{error, info};

use codegen_proto::{
    interpreter_service_client::InterpreterServiceClient as CodegenClient,
    InterpreterRequest as CodegenRequest,
    InterpreterResponse as CodegenResponse,
};

pub mod codegen_proto {
    tonic::include_proto!("interpreter");
}

#[derive(Debug, serde::Serialize)]
struct CodegenResult {
    id: i32,
    #[serde(rename = "result")]
    target_code: String,
    status: bool,
    error: String,
    #[serde(rename = "createdAt")]
    created_at: String,
}

#[derive(Debug, serde::Serialize)]
struct ImportingResult {
    target_code: String,
    status: bool,
    error: String,
}

impl From<CodegenResponse> for CodegenResult {
    fn from(response: CodegenResponse) -> Self {
        CodegenResult {
            id: 1,
            target_code: response.output,
            status: response.status,
            error: response.error,
            created_at: Utc::now().to_rfc3339(),
        }
    }
}

impl From<CodegenResponse> for ImportingResult {
    fn from(response: CodegenResponse) -> Self {
        ImportingResult {
            target_code: response.output,
            status: response.status,
            error: response.error,
        }
    }
}

impl From<ImportingResult> for HttpResponse {
    fn from(result: ImportingResult) -> Self {
        if result.status {
            HttpResponse::Ok()
                .content_type(ContentType::plaintext())
                .body(result.target_code)
        } else {
            HttpResponse::build(StatusCode::INTERNAL_SERVER_ERROR)
                .content_type(ContentType::json())
                .body(serde_json::to_string(&result).unwrap())
        }
    }
}

#[derive(Debug, serde::Deserialize)]
struct Codegen {
    code: String,
}

#[derive(Debug, serde::Deserialize)]
struct ImportLib {
    filename: String,
}



#[actix_web::post("/codegen")]
async fn run_codegen(data: web::Data<AppData>, codegen: web::Json<Codegen>) -> HttpResponse {
    info!(target: "proxy", "Request received: {codegen:?}");

    let mut client = data
        .codegen_client
        .lock()
        .expect("Failed to lock codegen client");
    let request = tonic::Request::new(CodegenRequest {
        code: codegen.code.clone(),
    });
    let response = client
        .run(request)
        .await
        .expect("Failed to run codegen service");
    let message = response.into_inner();
    let result = CodegenResult {
        target_code: message.output,
        id: 1,
        status: message.status,
        error: message.error,
        created_at: Utc::now().to_rfc3339(),
    };
    HttpResponse::Ok().json(result)
}

#[actix_web::post("/lib")]
async fn import_lib(
    lib_path: web::Json<ImportLib>,
    data: web::Data<AppData>,
) -> HttpResponse {
    let path: PathBuf = lib_path.into_inner().filename.parse().unwrap();
    let base_path = data.lib_path.clone();
    let lib_path = base_path.join(path.clone());
    log::warn!("Importing lib: {:?}", lib_path);
    let content = fs::read_to_string(lib_path.clone());
    if content.is_err() {
        log::error!("File not found: {:?}", lib_path);
        return HttpResponse::from(ImportingResult {
            target_code: "".to_string(),
            status: false,
            error: "File not found".to_string(),
        });
    }
    let request = tonic::Request::new(CodegenRequest { code: content.unwrap() });
    let mut client = data
        .codegen_client
        .lock()
        .expect("Failed to lock codegen client");
    let response = client
        .run(request)
        .await
        .expect("Failed to run codegen service");
    HttpResponse::from(ImportingResult::from(response.into_inner()))
}

#[actix_web::get("/file/{filename:.*}")]
async fn import_file(req: HttpRequest) -> actix_web::Result<NamedFile> {
    let path: PathBuf = req.match_info().query("filename").parse().unwrap();
    Ok(NamedFile::open(path)?)
}

#[actix_web::get("/{file:.*}")]
async fn index(path: web::Path<(String,)>) -> HttpResponse {
    let client = awc::Client::default();
    let (file,) = path.into_inner();
    let base_uri = std::env::var("CLIENT_URI").expect("CLIENT_URI is not set");
    let mut raw_response = client
        .get(format!("{}/{}", base_uri, file))
        .insert_header(("User-Agent", "Actix-web"))
        .send()
        .await
        .unwrap();
    match raw_response
        .body()
        .limit(10_000_000) // Limit size of response to 10MB
        .await
    {
        Ok(body) => HttpResponse::Ok().body(body),
        Err(e) => {
            error!("Error: {:?}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

struct AppData {
    codegen_client: Mutex<CodegenClient<tonic::transport::channel::Channel>>,
    lib_path: PathBuf,
}

#[actix_web::main]
async fn main() -> Result<(), std::io::Error> {
    let data = web::Data::new(AppData {
        codegen_client: Mutex::new(
            CodegenClient::connect(std::env::var("CODEGEN_URI").expect("CODEGEN_URI is not set"))
                .await
                .expect("Failed to connect to codegen service"),
        ),
        lib_path: env::current_dir()
            .unwrap()
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf(),
    });

    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    HttpServer::new(move || {
        App::new()
            .app_data(data.clone())
            .wrap(Cors::permissive())
            .wrap(Logger::default())
            .service(index)
            .service(import_lib)
            .service(run_codegen)
    })
    .bind(("127.0.0.1", 3030))?
    .run()
    .await
}
