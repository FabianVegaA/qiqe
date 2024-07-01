import { Box, Typography } from "@mui/material";
import MarkdownPage from "../components/markdown";

export default function Home() {
  const source =
    "https://raw.githubusercontent.com/FabianVegaA/qiqe/main/README.md";
  return (
    <Box sx={{ m: 2, p: 2 }}>
      <Typography
        variant="h1"
        sx={{
          textAlign: "center",
        }}
      >
        Welcome to Qiqe!
      </Typography>
      <MarkdownPage source={source} />
    </Box>
  );
}
