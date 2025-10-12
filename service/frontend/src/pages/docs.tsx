import { Box } from "@mui/material";
import MarkdownPage from "../components/markdown";

export default function Docs() {
  const source =
    "https://raw.githubusercontent.com/FabianVegaA/qiqe/main/doc/README.md";
  return (
    <Box sx={{ m: "10rem", p: 2 }}>
      <MarkdownPage source={source} className="docs" />;
    </Box>
  );
}
