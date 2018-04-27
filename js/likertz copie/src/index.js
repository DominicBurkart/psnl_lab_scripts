import React from 'react';
import ReactDOM from 'react-dom';
import { StyletronProvider } from "styletron-react"
import Styletron from "styletron-client"
import App from './App';
//import registerServiceWorker from './registerServiceWorker';

document.getElementById('root').style["font-family"] = "'Helvetica', sans-serif"

ReactDOM.render(
  <StyletronProvider styletron={new Styletron()}>
    <App />
  </StyletronProvider>,
  document.getElementById('root'));
//registerServiceWorker();
