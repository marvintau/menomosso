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
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
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
