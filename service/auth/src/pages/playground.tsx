import { useContext, useState } from "react";
import AceEditor from "react-ace";
import "brace/mode/lisp";
import "brace/theme/tomorrow";
import "brace/theme/tomorrow_night";

import { postCode, importLibs } from "../api/run-code";
import codeEvaluate from "../lib/code-eval";
import {
  Autocomplete,
  Box,
  Divider,
  IconButton,
  List,
  ListItem,
  Stack,
  TextField,
  Typography,
} from "@mui/material";
import CircleIcon from "@mui/icons-material/Circle";
import DeleteIcon from "@mui/icons-material/Delete";
import CodeIcon from "@mui/icons-material/Code";
import PlayArrowIcon from "@mui/icons-material/PlayArrow";
import { useTheme } from "@mui/material/styles";
import { ColorModeContext } from "./root";

type Result = {
  id: number;
  status: boolean;
  error: string;
  result: string;
  createdAt: string;
};

type ImportOption = {
  label: string;
  dir: string;
};

const base: ImportOption = {
  label: "base",
  dir: "qiqe/base.qq",
};

type Import = {
  target: string;
};

export default function Playground() {
  const mode = useContext(ColorModeContext);
  const theme = useTheme();
  const [code, setCode] = useState("");
  const [result, setResult] = useState([] as Result[]);
  const [importOptions, setImportOptions] = useState([] as ImportOption[]);

  const addResult = (res: Result) => setResult([res, ...result]);
  const clearResult = () => setResult([]);

  const setErr = (err: string) => {
    addResult({
      id: result.length,
      status: false,
      error: err || "Something went wrong. Please try again later.",
      result: "",
      createdAt: new Date().toDateString(),
    });
  };

  const runCode = async (code: string) => {
    const imports = await importLibs(importOptions.map((i) => i.dir)).catch(
      setErr
    );
    const res = await postCode(code).catch(setErr);
    if (!res) return;

    await res
      .json()
      .then((data: Result) => {
        if (!data.status) {
          addResult(data);
          return;
        }

        codeEvaluate({ code: data.result, imports: imports || [] })
          .then((stdout: string) => {
            addResult({
              id: data.id,
              status: data.status,
              error: data.error,
              result: stdout,
              createdAt: data.createdAt,
            });
          })
          .catch((stderr: string) => {
            addResult({
              id: data.id,
              status: data.status && !stderr,
              error: data.error || stderr,
              result: "",
              createdAt: data.createdAt,
            });
          });
      })
      .catch(setErr);
  };

  const handleRun = (e: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    e.preventDefault();
    if (!code) return;
    runCode(code);
  };

  return (
    <Box>
      <Stack direction="column" sx={{ px: 2 }}>
        <Stack
          direction="row"
          sx={{
            gap: 2,
            pt: 2,
            display: "flex",
            justifyContent: "flex-start",
            // px: 2,
          }}
        >
          <IconButton aria-label="run" size="large" onClick={handleRun}>
            <PlayArrowIcon fontSize="inherit" />
          </IconButton>
          <IconButton aria-label="delete" size="large" onClick={clearResult}>
            <DeleteIcon fontSize="inherit" />
          </IconButton>

          <Autocomplete
            multiple
            id="import-select"
            options={[base]}
            getOptionLabel={(option) => option.label}
            defaultValue={[base]}
            onChange={(_, value) => setImportOptions(value)}
            renderInput={(params) => (
              <TextField {...params} label="Import" placeholder="Import" />
            )}
            sx={{ width: 1 }}
          />
        </Stack>

        <Stack
          direction="row"
          sx={{
            display: "flex",
            justifyContent: "center",
            gap: 2,
            pt: 2,
            width: 1,
          }}
        >
          <Stack
            sx={{
              borderColor: theme.palette.divider,
              borderStyle: "solid",
              mb: 4,
              width: 1 / 2,
            }}
          >
            <AceEditor
              mode="lisp"
              theme={
                theme.palette.mode === "dark" ? "tomorrow_night" : "tomorrow"
              }
              onChange={setCode}
              name="code-editor"
              height="100vh"
              width="100%"
              fontSize={20}
            />
          </Stack>
          <Divider
            orientation="vertical"
            flexItem
            sx={{ borderColor: theme.palette.divider, height: "100vh" }}
          >
            <IconButton
              aria-label="resize"
              size="medium"
              onDrag={(e) => e.preventDefault()}
            >
              <CodeIcon fontSize="inherit" />
            </IconButton>
          </Divider>
          <List
            sx={{
              width: 1 / 2,
              height: "100vh",
              backgroundColor:
                theme.palette.mode === "dark"
                  ? "#25282c"
                  : theme.palette.background.paper,
              color: theme.palette.text.primary,
              borderColor: theme.palette.divider,
              borderStyle: "solid",
              pb: 0,
            }}
          >
            {result.map(({ id, status, error, result }) => (
              <ListItem key={id} sx={{ pt: 0 }}>
                <CircleIcon
                  fontSize="small"
                  color={status ? "primary" : "error"}
                  sx={{
                    display: "flex",
                    alignItems: "flex-start",
                    justifyContent: "center",
                    fontSize: "14px",
                    borderRadius: "0",
                    background: "transparent",
                  }}
                />
                <Typography component="pre" sx={{ ml: 2, width: 1 }}>
                  {status ? result.toString() : error.toString()}
                </Typography>
              </ListItem>
            ))}
          </List>
        </Stack>
      </Stack>
    </Box>
  );
}
