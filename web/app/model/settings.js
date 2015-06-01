// @flow
declare var SETTINGS;

import {User} from './user'

export type AppSettings = {
  appName: string;
  user: User;
  version: string;
  endpoint: string;
}

export function readSettings():AppSettings {
  return SETTINGS;
}
