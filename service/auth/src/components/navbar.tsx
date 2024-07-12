import { AppBar, Box, Button, Stack, Toolbar, Typography } from "@mui/material";
import IconButton from "@mui/material/IconButton";
import { useTheme } from "@mui/material/styles";
import { useContext } from "react";
import { DarkMode, LightMode } from "@mui/icons-material";
import { ColorModeContext } from "../pages/root";
import QiqeIcon from "./qiqeIcon";

const pages = [
  {
    title: "Playground",
    href: "/playground",
  },
  {
    title: "Docs",
    href: "/docs",
  },
];

export default function Navbar() {
  const theme = useTheme();
  const colorMode = useContext(ColorModeContext);

  return (
    <AppBar position="static" color="primary">
      <Toolbar disableGutters>
        <Box
          sx={{
            display: "flex",
            justifyContent: "left",
            width: "100%",
          }}
        >
          <Box
            sx={{
              display: "flex",
              alignItems: "center",
              ml: 1,
            }}
          >
            <QiqeIcon
              viewBox="0 0 512 512"
              sx={{
                width: 40,
                height: 40,
                fill: "white",
                mr: 1,
                pl: 1,
                pb: 1,
                alignSelf: "flex-start",
              }}
            />

            <Typography
              component="a"
              href="/"
              sx={{
                mr: 2,
                fontFamily: "monospace",
                fontWeight: 700,
                letterSpacing: ".3rem",
                color: "inherit",
                textDecoration: "none",
              }}
            >
              Qiqe
            </Typography>
          </Box>

          <Stack direction="row" spacing={2}>
            {pages.map((page) => (
              <Button
                key={page.title}
                onClick={() => {
                  window.location.href = page.href;
                }}
                sx={{ my: 2, color: "white", display: "block" }}
              >
                {page.title}
              </Button>
            ))}
          </Stack>
          <Box
            sx={{
              ml: 1,
              pr: 1,
              flexGrow: 1,
              display: "flex",
              justifyContent: "flex-end",
            }}
          >
            <IconButton onClick={colorMode.toggleColorMode} color="inherit">
              {theme.palette.mode === "dark" ? <LightMode /> : <DarkMode />}
            </IconButton>
          </Box>
        </Box>
      </Toolbar>
    </AppBar>
  );
}
