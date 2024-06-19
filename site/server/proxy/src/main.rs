use std::sync::Mutex;

use actix_web::{web, App, HttpResponse, HttpServer};
use actix_cors::Cors;
use chrono::Utc;
use log::info;

use codegen_proto::{
    interpreter_service_client::InterpreterServiceClient as CodegenClient, 
    InterpreterRequest as CodegenRequest
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

#[derive(Debug, serde::Deserialize)]
struct Codegen { code: String }

#[actix_web::post("/codegen")]
async fn run_codegen(
    data: web::Data<AppData>,
    codegen: web::Json<Codegen>
) -> HttpResponse {
    info!(target: "proxy", "Request received: {codegen:?}");

    let mut client = data.codegen_client.lock().expect("Failed to lock codegen client");
    let request = tonic::Request::new(CodegenRequest { code: codegen.code.clone() });
    let response = client.run(request).await.expect("Failed to run codegen service");
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

struct AppData {
    codegen_client: Mutex<CodegenClient<tonic::transport::channel::Channel>>,
}

#[actix_web::main]
async fn main() -> Result<(), std::io::Error> {
    let data = web::Data::new(AppData {
        codegen_client: Mutex::new(
            CodegenClient::connect(std::env::var("CODEGEN_URI").expect("CODEGEN_URI is not set"))
            .await
            .expect("Failed to connect to codegen service")
        ),
    });

    HttpServer::new(move || {
        App::new()
            .app_data(data.clone())
            .wrap(Cors::permissive())
            .service(run_codegen)      
    })
    .bind(("127.0.0.1", 3030))?
    .run()
    .await
}