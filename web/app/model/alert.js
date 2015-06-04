// @flow

import {Promise} from 'es6-promise'
import {EventEmitter} from 'events'

// AlertModel //////////////////////////////////////
export type Alert = {
  message: string;
  type: string;
}

export type AlertType = "success" | "error" | "info" | "secondary"

// methods for alerts
export class AlertModel {

  alert: Alert;

  // resist one url change
  persist: boolean;

  events: EventEmitter;

  constructor() {
    this.alert = this.emptyAlert()
    this.events = new EventEmitter()
  }

  //// Alert ////////////////////////////////
  update(type:AlertType, message:string, persist:boolean = false):Alert {
    this.alert = {type, message}
    this.persist = persist
    this.events.emit('change', this)
    return alert
  }

  oops():Alert {
    return this.update("error", "Something is broken. Please email support at serials@orbit.al and we'll take a look")
  }

  clear():Alert {
    this.alert = this.emptyAlert()
    this.events.emit('change', this)
    return this.alert
  }

  urlChange():void {
    if (this.persist) {
      this.persist = false
      return
    }

    this.clear()
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

