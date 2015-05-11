// @flow

import Promise from 'bluebird'
import {EventEmitter} from 'events'

// AlertModel //////////////////////////////////////
export type Alert = {
  message: string;
  type: string;
}

// methods for alerts
export class AlertModel {

  alert: Alert;

  events: EventEmitter;

  constructor() {
    this.alert = this.emptyAlert()
    this.events = new EventEmitter()
  }

  //// Alert ////////////////////////////////
  update(alert:Alert):Alert {
    this.alert = alert
    this.events.emit('change', this)
    return alert
  }

  clear():Alert {
    this.alert = this.emptyAlert()
    this.events.emit('change', this)
    return this.alert
  }

  emptyAlert():Alert {
    return {
      message: '',
      type: ''
    }
  }

  //// Changes //////////////////////////////
  bind(f:Function) {
    this.events.on('change', f)
  }

}

export var Alerts = new AlertModel()

