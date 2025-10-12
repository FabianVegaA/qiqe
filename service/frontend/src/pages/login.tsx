export default function Login() {
  return (
    <div id="login" className="container">
      <h1>Login</h1>

      <form action="">
        <label htmlFor="">
          <p>Username</p>
          <input type="text" />
        </label>
        <label htmlFor="">
          <p>Password</p>
          <input type="password" />
        </label>
        <div className="submit">
          <button type="submit">Submit</button>
        </div>
      </form>
    </div>
  );
}
