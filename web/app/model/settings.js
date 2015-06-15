// @flow

import {User} from './user'

export var EMAIL = "webfiction@orbit.al"

export type AppSettings = {
  appName: string;
  user: User;
  version: string;
  appEndpoint: string;
  appEnvironment: string;
}

declare var SETTINGS:AppSettings;

export function settings():AppSettings {
  return SETTINGS;
}

export function isProduction():boolean {
  return SETTINGS.appEnvironment === "Production"
}
