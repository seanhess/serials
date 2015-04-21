
// @flow

var React = require('react')

var {RouteHandler} = require('react-router')
var {MainContainer, Header} = require('../layout/main')

export class Admin extends React.Component {
  render() {
    return <div>
      <Header />
      <MainContainer>
        <RouteHandler {...this.props}/>
      </MainContainer>
    </div>
  }
}
