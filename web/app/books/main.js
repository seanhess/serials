// @flow

var React = require('react')

var {RouteHandler} = require('react-router')
var {MainContainer, Header} = require('../layout/main')

export class Main extends React.Component {
  render():React.Element {
    return <div>
      <Header {...this.props}/>
      <MainContainer>
        <RouteHandler {...this.props}/>
      </MainContainer>
    </div>
  }
}
