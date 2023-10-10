export default function Signup() {
  return (
    <div id="signup" className="container">
      <h1>Signup</h1>

      <form action="">
        <label htmlFor="">
          <p>Username</p>
          <input type="text" />
        </label>
        <label htmlFor="">
          <p>Password</p>
          <input type="password" />
        </label>
        <label htmlFor="">
          <p>Confirm Password</p>
          <input type="password" />
        </label>
        <div className="submit">
          <button type="submit">Submit</button>
        </div>
      </form>
    </div>
  );
}
