
// @flow

var React = require('react')

var {RouteHandler} = require('react-router')
var {MainContainer, Header} = require('../layout/main')
var {AdminModel} = require('../model')

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

export class ImportLog extends React.Component {

  static load(params) {
    return AdminModel.importLog(params.n)
    .then(function(log) {
      return {text: log.text}
    })
  }


  render() {
    var numLines = parseInt(this.props.params && this.props.params.n, 10)
    var moreUrl = "#/admin/import-log/" + (numLines+500)

    return <div>
      <h2>Import Log</h2>
      <div><pre style={{fontSize: '14px'}}>{this.props.text}</pre></div>
      <a href={moreUrl}>More</a>
    </div>
  }
}
