
// @flow

var React = require('react')

var {RouteHandler, Link} = require('react-router')

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
  renderCurrentUser() {
    var currentUser = this.props.currentUser;

    if (currentUser) {
      return <div style={{display: 'inline-block'}}>
        <p style={LinkStyle}>Hello, {this.props.currentUser.firstName}</p>
        <a style={LinkStyle} onClick={this.props.logout}>Logout</a>
      </div>
    }

    else {
      return <div style={{display: 'inline-block'}}>
        <Link style={LinkStyle} to='login'>Login</Link>
        <Link style={LinkStyle} to='signup'>Signup</Link>
      </div>
    }
  }

  render() {
    return <nav style={NavBar} role="navigation">
      <div style={{float: 'right'}}>
        {this.renderCurrentUser()}
        <Link style={LinkStyle} to="about">About</Link>
        <Link style={LinkStyle} to="sources">Admin</Link>
      </div>
      <div style={CenterText}><Link to="books" style={TitleStyle}>Serials</Link></div>
    </nav>
  }
}

