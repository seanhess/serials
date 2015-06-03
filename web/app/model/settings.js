// @flow
declare var SETTINGS;

import {User} from './user'

export type AppSettings = {
  appName: string;
  user: User;
  version: string;
  appEndpoint: string;
}

export function settings():AppSettings {
  return SETTINGS;
}

export function isProduction():boolean {
  return SETTINGS.appEndpoint == "http://serials.orbit.al"
}
