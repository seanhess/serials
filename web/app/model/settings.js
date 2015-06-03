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
