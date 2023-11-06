import MarkdownPage from "../components/markdown";

export default function Home() {
  const source =
    "https://raw.githubusercontent.com/FabianVegaA/qiqe/main/README.md";
  return (
    <>
      <div className="docs">
        <h1>Welcome to Qiqe!</h1>
      </div>
      <MarkdownPage source={source} className="docs" />;
    </>
  );
}
