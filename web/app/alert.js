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
      type: type,
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

    var style = {
      position: 'fixed',
      bottom: 0,
      width: '100%',
      margin: 0,
      fontSize: 16
    }

    return <div data-alert className={classes} style={style}>
      <span>{face(this.state.type)}</span>
      <span style={{marginLeft: 20}}>{this.state.message}</span>
      <a onClick={this.onClick} className="close">×</a>
    </div>
  }
}

function typeClass(type:string):string {
  if (type === "error") return "alert"
  else return type
}

// http://wrttn.me/30dbfd/
function face(type:string):string {
  if (type === "error")        return "◉︵◉"
  else if (type === "success") return "(/◔ ◡ ◔)/"
  // ◕︵◕
  // ಠ╭╮ಠ
  // ◉︵◉
  // ⊙﹏⊙
  // ⊙_ʘ
  // ಠ_ಠ
  else return ""
}
