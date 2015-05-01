// @flow

export var updateLocalStorage = function(key, data) {
    return localStorage.setItem(key, JSON.stringify(data))
}


export var getLocalStorage = function(key) {
    return JSON.parse(localStorage.getItem(key))
}

