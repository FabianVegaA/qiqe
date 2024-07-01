import { Outlet } from "react-router-dom";
import Navbar from "../components/navbar";
import { Box } from "@mui/material";
import { createTheme, ThemeProvider, useTheme } from "@mui/material/styles";
import { createContext, useMemo, useState } from "react";

export const ColorModeContext = createContext({ toggleColorMode: () => {} });

function Root() {
  return (
    <Box
      sx={{
        bgcolor: "background.default",
        color: "text.primary",
        height: "100%",
      }}
    >
      <Navbar />
      <Outlet />
    </Box>
  );
}

export default function ToggleColorMode() {
  const [mode, setMode] = useState<"light" | "dark">("light");
  const colorMode = useMemo(
    () => ({
      toggleColorMode: () => {
        setMode((prevMode: "light" | "dark") =>
          prevMode === "light" ? "dark" : "light"
        );
      },
    }),
    []
  );

  const theme = useMemo(
    () =>
      createTheme({
        palette: {
          mode,
        },
      }),
    [mode]
  );

  return (
    <ColorModeContext.Provider value={colorMode}>
      <ThemeProvider theme={theme}>
        <Root />
      </ThemeProvider>
    </ColorModeContext.Provider>
  );
}
