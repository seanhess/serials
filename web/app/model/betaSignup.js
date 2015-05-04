// @flow

import {Get, Post, Put, Del, url} from '../api'


// BetaSignupModel /////////////////////////////////////


export type BetaSignup = {
  id: string;
  email: string;
}

export var BetaSignupModel = {
  create(betaSignup:BetaSignup) {
    betaSignup.id = ""
    return Post(url('beta-signup'), betaSignup)
  }
}

