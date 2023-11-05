import MarkdownPage from "../components/markdown";

export default function Docs() {
  const source =
    "https://raw.githubusercontent.com/FabianVegaA/qiqe/main/doc/README.md";
  return <MarkdownPage source={source} className="docs" />;
}
