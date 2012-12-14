<html>
  <head>
    <title>Snaplet Fay Example Application</title>
    <link rel="stylesheet" type="text/css" href="/static/screen.css"/>
    <script src="/static/jquery.js"></script>
    <script src="/static/underscore.js"></script>
    <script src="/static/index.js"></script>
    <script src="/fay/Index.js"></script>
  </head>
  <body>
    <div id="content">
      <h1>Snaplet Fay Example Application</h1>

      <div>Current time:</div>
      <div id="current-time"><current-time/></div>

      <br>
      <div id="loginStatus" class="status">
        <ifLoggedIn>
          You're logged in.
        </ifLoggedIn>
        <ifLoggedOut>
          You're not logged in.
        </ifLoggedOut>
      </div>
      <input id="viewLoginForm" type="button" value="Log in">
      <input id="viewRegisterForm" type="button" value="Register">
      <input id="logout" type="button" value="Log out">

      <div id="formContainer">
      </div>

      <br>
      <div><a href="/static/broken.html">See what happens when you try to load a html file referencing a Fay file containing Syntax errors.</a></div>
      
      <br>
    </div>
  </body>
</html>
