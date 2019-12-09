'use strict';
import * as config from './firebase.js';
import * as firebase from "firebase/app";
import "firebase/auth";

console.log(config)
// Add the Firebase products that you want to use
firebase.initializeApp(config.default);
// Firebase App (the core Firebase SDK) is always required and must be listed first
require("./styles.scss");

const {Elm} = require('./Main');
var app = Elm.Main.init({flags: 6});

// ログイン監視
app.ports.signIn.subscribe(_ => {
  firebase.auth().signInWithPopup(provider).then((_) => {}).catch((error) => {});
});

firebase.auth().onAuthStateChanged((user) => {
  if (user) {
      user.getIdToken(false).then((token) => app.ports.signedIn.send(token));
  }
})

// Use ES2015 syntax and let Babel compile it for you
var testFn = (inp) => {
    let a = inp + 1;
    return a;
}

const provider = new firebase.auth.GoogleAuthProvider();