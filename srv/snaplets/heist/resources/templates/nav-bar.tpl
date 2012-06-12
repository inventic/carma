<!-- Navigation bar on top -->
<div class="navbar navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">
      <ul class="nav">
        <a class="brand" href="/">
          CaRMa
        </a>
        <li class="divider-vertical" />
        <li id="call-screen-nav">
          <a href="#call">Приём звонка</a>
        </li>
        <li id="case-screen-nav">
          <a href="#case">Кейс</a>
        </li>
        <li id="back-screen-nav">
          <a href="#back">Бэкофис</a>
        </li>
        <li id="search-screen-nav">
          <a href="#search">Поиск</a>
        </li>
        <li class="dropdown">
          <a href="#" class="dropdown-toggle" data-toggle="dropdown">
            Ещё <b class="caret"></b>
          </a>
          <ul class="dropdown-menu">
            <li id="vin-screen-nav">
              <a href="#vin">Обновление базы VIN</a>
            </li>
            <li id="partner-screen-nav">
              <a href="#partner">Редактирование партнёров</a>
            </li>
          </ul>
        </li>
      </ul>
      <ifLoggedIn>
        <ul class="nav pull-right">
          <li class="divider-vertical" />
          <li class="dropdown">
            <a href="#"
               class="dropdown-toggle"
               data-toggle="dropdown">
              <i class="icon-user icon-white" />&nbsp;<loggedInUser />
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu">
              <li>
                <a href="/logout/">
                  <i class="icon-off icon-black" />&nbsp;Выход
                </a>
            </ul>
          </li>
        </ul>
      </ifLoggedIn>
    </div>
  </div>
</div>
