
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

var TitleStyle = {
  color: 'white',
  fontSize: '18px',
  margin: 0,
}

var LinkStyle = {
  color: 'white',
  fontSize: '14px',
  margin: 14,
  display: 'inline-block'
}

var CenterText = {
  padding: 10,
  margin: 0,
}

var NavBar = {
  backgroundColor: '#333',
  height: 47,
  position: 'relative'
}

export class Header extends React.Component {
  render() {
    return <nav style={NavBar} role="navigation">
      <div style={{float: 'right'}}>
        <a style={LinkStyle} href="#/pages/about">About</a>
        <a style={LinkStyle} href="#/admin/sources">Admin</a>
      </div>
      <div style={CenterText}><a href="#" style={TitleStyle}>Serials</a></div>
    </nav>
  }
}


