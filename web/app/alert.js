// @flow
import React from 'react'
import {Alerts} from './model/alert'

export class AlertView extends React.Component {

  constructor(props:any) {
    super(props)
    this.state = this.props.alert
  }

  componentWillReceiveProps(newProps:any) {
    this.handleState(newProps)
  }

  handleState(props:any) {
    if (!props.alert) return false
    var message = props.alert.message
    var type = props.alert.type || 'error'

    this.setState({
      message: message,
      type: type
    })
  }

  onClick() {
    Alerts.clear()
  }

  render():React.Element {
    if (!this.state.message) {
      return <div/>
    }

    var classes = ['alert-box', typeClass(this.state.type)].join(' ')

    return <div data-alert className={classes} style={{position: 'fixed', bottom: 0, width: '100%', margin: 0}}>
      {this.state.message}
      <a onClick={this.onClick} className="close">×</a>
    </div>
  }
}

function typeClass(type:string):string {
  if (type === "error") return "alert"
  else return type
}

