import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import 'whatwg-fetch';

class App extends Component {
  render() {
    return (
      <div className="App">
        <div className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
        </div>
        <Body/>
      </div>
    );
  }
}

class Body extends Component {
  render() {

    let res = fetch('http://everstream.cn:1337/api/get_player_list', {
      mode: 'no-cors',
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Origin': 'http://everstream.cn:3000',
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'X-Requested-With',
        'Access-Control-Request-Origin': 'http://everstream.cn:3000'
      },
      body: JSON.stringify({})
    })
    .then(response => response.json())
    .catch(error => {
      console.log(error);
      return error
    })

    console.log(res);

    return (<div/>)

  }
}

export default App;
