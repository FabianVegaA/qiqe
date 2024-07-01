import { useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";

export default function MarkdownPage(props: {
  source: string;
  className?: string;
}) {
  const [markdown, setMarkdown] = useState("");
  useEffect(() => {
    fetch(props.source)
      .then((res) => res.text())
      .then((text) => setMarkdown(text))
      .catch((err) => console.error(err));
  }, [props.source]);

  if (!markdown) {
    return <div>Loading...</div>;
  }

  return <ReactMarkdown>{markdown}</ReactMarkdown>;
}
