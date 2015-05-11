// @flow
import React from 'react'
import {Alerts} from './model/alert'

export class AlertView extends React.Component {

  _timer: any;

  constructor(props:any) {
    super(props)
    this.state = this.props.alert
    this._timer = null
  }

  componentWillReceiveProps(newProps:any) {
    this.handleState(newProps)
    this.handleTimeout()
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

  handleTimeout() {
    this._timer != null ? clearTimeout(this._timer) : null
    this._timer = setTimeout(() => {Alerts.clear()}, 10000)
  }

  onClick() {
    Alerts.clear()
  }

  render():React.Element {
    // TODO: this could be more robust only supports success and error now
    var backgroundColor = this.state.type == 'success' ? '#008000' : '#ff0000'
    if (this.state.message) {
      return <div style={{position: 'fixed', height: '47px', width: '100%', zIndex: '100', backgroundColor: backgroundColor}}>
        <a style={{float: 'right', color: '#fff', padding: '10px 15px'}} onClick={this.onClick}>x</a>
        <p style={{color: '#fff', textAlign: 'center', paddingTop: '10px'}}>{this.state.message}</p>
      </div>
    }

    else {
      return null
    }
  }

}

