
// @flow

var React = require('react')

var {RouteHandler} = require('react-router')

export class MainContainer extends React.Component {
  render() {
    return <div className="row columns small-12">
      {this.props.children}
    </div>
  }
}

export class Header extends React.Component {
  render() {
    return <nav className="top-bar" data-topbar role="navigation">
      <ul className="title-area">
        <li className="name">
          <h1><a href="#">Serials</a></h1>
        </li>
      </ul>

      <section className="top-bar-section">
        <ul className="right">
          <li><a href="#/admin/sources">Admin</a></li>
          {/*<li className="has-dropdown">
            <a href="#">Right Button Dropdown</a>
            <ul className="dropdown">
              <li><a href="#">First link in dropdown</a></li>
              <li className="active"><a href="#">Active link in dropdown</a></li>
            </ul>
          </li>*/}
        </ul>

        {/*
        <ul className="left">
          <li><a href="#">Left Nav Button</a></li>
        </ul>
        */}
      </section>
    </nav>
  }
}
