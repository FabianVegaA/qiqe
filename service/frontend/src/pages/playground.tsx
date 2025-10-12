import { useEffect, useState } from "react";
import AceEditor from "react-ace";
// import "brace/mode/lisp";
import "../lib/ace-qiqe-mode";
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
import { useShell } from "../hooks/useShell";

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
  dir: "qiqe/library/std.qq",
};

type Import = {
  label: string;
  target: string;
};

export default function Playground() {
  const theme = useTheme();
  const [code, setCode] = useState("");
  const [importeds, setImporteds] = useState([] as Import[]);

  const { shell, print, raise, clear } = useShell();

  const defaultImports = [
    base,
    {
      label: "list",
      dir: "qiqe/library/list.qq",
    },
  ];

  useEffect(() => {
    const fetchImports = async () => {
      const compiledBase = await importLibs(
        defaultImports.map((lib) => lib.dir)
      );
      if (!compiledBase) return;
      setImporteds([{ label: base.label, target: compiledBase.join("\n") }]);
    };
    fetchImports();
  }, []);

  const handleImport = async (libs: string[]) => {
    const compiled = await importLibs(libs);
    if (!compiled) return;
    setImporteds([
      ...importeds,
      ...libs.map((lib) => ({ label: lib, target: compiled.join("\n") })),
    ]);
  };

  const runCode = async (code: string) => {
    const res = await postCode(code).catch((reason) => raise(reason, 1));
    if (!res) return;

    await res
      .json()
      .then((data: Result) => {
        if (!data.status) {
          raise(data.error, 1);
          return;
        }

        codeEvaluate({
          print,
          raise,
          code: data.result,
          imports: importeds.map((i) => i.target),
        });
      })
      .catch((reason) => raise(reason, 1));
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
          <IconButton aria-label="delete" size="large" onClick={clear}>
            <DeleteIcon fontSize="inherit" />
          </IconButton>

          <Autocomplete
            multiple
            id="import-select"
            options={defaultImports}
            getOptionLabel={(option) => option.label}
            isOptionEqualToValue={(option, value) =>
              option.label === value.label
            }
            defaultValue={[base]}
            onChange={(_, value) => {
              if (!value) return;
              handleImport(value.map((v) => v.dir));
            }}
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
              mode="qiqe"
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
              overflow: "auto",
              backgroundColor:
                theme.palette.mode === "dark"
                  ? "#25282c"
                  : theme.palette.background.paper,
              color: theme.palette.text.primary,
              borderColor: theme.palette.divider,
              borderStyle: "solid",
              pb: 0,
              height: "100vh",
            }}
          >
            {Array.from(shell.entries()).map(
              ([index, [statusCode, output]]) => (
                <ListItem key={index} sx={{ pt: 0 }}>
                  <CircleIcon
                    fontSize="small"
                    color={statusCode == 0 ? "primary" : "error"}
                    sx={{
                      mt: 1,
                      display: "flex",
                      alignItems: "flex-start",
                      alignSelf: "self-start",
                      justifyContent: "center",
                      fontSize: "14px",
                      borderRadius: "0",
                      background: "transparent",
                    }}
                  />
                  <Typography
                    component="pre"
                    noWrap
                    sx={{
                      ml: 2,
                      width: 1,
                      scrollbarWidth: "initial",
                      fontFamily: "monospace",
                      textOverflow: "inherit",
                      whiteSpace: "pre-wrap",
                    }}
                  >
                    {output}
                  </Typography>
                </ListItem>
              )
            )}
          </List>
        </Stack>
      </Stack>
    </Box>
  );
}
