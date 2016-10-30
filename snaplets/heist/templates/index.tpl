<!DOCTYPE html>
<html>
  <head>
    <title>Eksercise</title>
    <link rel="stylesheet" type="text/css" href="static/klarna-ui-css-components.css">
    <link rel="icon" type="image/png" href="/static/favicon.png">
  </head>
  <body>
    <div class="search">
      <div class="cui__input giant">
        <form action="/" class="form-inline" method="get">
          <input search-input type="text" name="q" class="cui__input__input" autofocus="autofocus" placeholder="Type your search query">
          <input type="submit" style="display:none" />
          <!--
          <input type="submit" class="btn btn-primary" value="Search">
          -->
        </form>
      </div>
    <div class="results">
      <div class="cui__selector--direct title">
        <h2 class="cui__selector--direct__title">Search results</h2>
        <people>
          <div class="cui__selector--direct__item">
            <img class="user-avatar" src="${avatar}" />
            <div class="cui__selector--direct__label">
              <name />
            </div>
            <p class="cui__selector--direct__description">
              <street /> <city />, <country />
            </p>
          </div>
        </people>
      </div>
    </div>
    </div>
  </body>
</html>
