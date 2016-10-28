<!DOCTYPE html>
<html>
  <head>
    <title>memoi.se</title>
    <link rel="stylesheet" type="text/css" href="static/bootstrap.min.css">
    <link rel="icon" type="image/png" href="/static/favicon.png">
  </head>
  <body>
    <div class="container-fluid">
      <div class="hero-unit">
        <form action="/" class="form-inline" method="get">
          <input search-input type="text" name="q" autofocus="autofocus" placeholder="Type your search query">
          <input type="submit" class="btn btn-primary" value="Search">
          <!--
          <input type="submit" style="display:none" />
          -->
        </form>
      </div>
  <people>
    <div class="results">
      <div class="cui__selector--direct title">
        <h2 class="cui__selector--direct__title">Search results</h2>
        <div class="cui__selector--direct__item">
          <img class="user-avatar" src="${avatar}" />
          <div class="cui__selector--direct__label">
            <name />
          </div>
          <p class="cui__selector--direct__description">
            <street /> <city />, <country />
          </p>
        </div>
      </div>
    </div>
  </people>
    </div>
  </body>
</html>
