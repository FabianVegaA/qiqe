import { Link } from "react-router-dom";

export default function Navbar() {
  return (
    <div id="navbar">
      <Link to="/">Home</Link>
      <Link to="/playground">Playground</Link>
      <Link to="/docs">Docs</Link>
      {/* <Link to="/login">Login</Link>
      <Link to="/signup">Signup</Link> */}
    </div>
  );
}
