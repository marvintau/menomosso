import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import 'whatwg-fetch';

import {PlayerDetail} from './PlayerDetail.js';
import {CardDetail} from './CardDetail.js';

import Select from 'react-select';
import {Col, ControlLabel, Button} from "react-bootstrap";

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

  constructor(){
    super()
    this.state = {
      playerList:[],
      players:[],
      remainingPlayers:[],
      cards:[],

      selectedSelf:{},
      selectedOppo:{},
      selectedSelfValue:{},
      selectedOppoValue:{}
    }
  }

  /**
   * 拿到所有玩家的列表
   * @return {[type]} [description]
   */
  getPlayerList(){
    fetch('http://everstream.cn:1337/api/get_player_list', {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Origin': 'http://everstream.cn:3000',
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'X-Requested-With'
      },
      body: JSON.stringify({})
    })
    .then(response => response.json() )
    .then(list =>{
      let options = list.map(elem => ({value:elem.id, label:elem.player_name}))
      this.setState({playerList:list, players:options})
    })
    .catch(error => {
      console.log(error);
      return error
    })
  }

  getPlayerProfile(selected){

    fetch('http://everstream.cn:1337/api/get_player', {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Origin': 'http://everstream.cn:3000',
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'X-Requested-With'
      },
      body: JSON.stringify({id: selected.value})
    })
    .then(response => response.json() )
    .then(res =>{
      this.setState((prevState) => {
        prevState.selectedSelfValue = selected;
        prevState.selectedSelf = res.player_profile;
        prevState.remainingPlayers = prevState.players.filter(playerOption => playerOption.value != selected.value),
        prevState.cards = res.card_profiles;
        prevState.selectedOppoValue = prevState.remainingPlayers[0];
        prevState.selectedOppo = prevState.playerList.filter(player_profile => player_profile.id == prevState.selectedOppoValue.value)[0]
        return prevState;
      });
    })
    .catch(error => {
      console.log(error);
      return error
    })
  }

  beginBattle(){
    console.log(this.state.selectedSelf.id);
    console.log(this.state.selectedOppo.id);
    console.log(this.state.selectedOppo.selected_skills);
    console.log(this.state.selectedOppo.preset_card_id);
    fetch('http://everstream.cn:1337/api/', {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Origin': 'http://everstream.cn:3000',
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'X-Requested-With'
      },
      body: JSON.stringify({
        id1: this.state.selectedSelf.id,
        id2: this.state.selectedOppo.id,
        self_card_id: this.state.selectedSelf.preset_card_id,
        skills : this.state.selectedSelf.selected_skills
      })
    })
    .then(response => response.json() )
    .then(res =>{
      console.log(res)
    })
    .catch(error => {
      console.log(error);
      return error
    })
  }

  onSelectSelf(selected){
    console.log("changed self player: "+selected.label);
    this.getPlayerProfile(selected)
  }

  onSelectOppo(selected){
    console.log("changed oppo: "+selected.label);
    this.setState((prevState) => {
      prevState.selectedOppoValue = selected;
      prevState.selectedOppo = prevState.playerList.filter(player_profile => player_profile.id == prevState.selectedOppoValue.value)[0]
      return prevState
    })
  }

  // 开始加载元素之前获得玩家列表
  componentWillMount(){
    this.getPlayerList()
  }

  onSelectCard(selected){
    console.log("changed card: "+selected.label);
    this.setState((prevState, props) => {
      prevState.selectedSelf.preset_card_id = selected.value;
      prevState.selectedSelf.selected_skills = "";
      return prevState;
    })
  }

  render() {

    return (
      <div>
      <Col className="container-fluid" md={3}>
        <ControlLabel>玩家名称</ControlLabel>
        < Select name="Yep" value={this.state.selectedSelfValue} options={this.state.players} onChange={this.onSelectSelf.bind(this)} clearable={false}/>
        < PlayerDetail cardProps={this.state.cards} playerProps={this.state.selectedSelf}/>
        <hr/>
        < CardDetail cardProps={this.state.cards} player={this.state.selectedSelf} onChange={this.onSelectCard.bind(this)}/>
        <hr/>
        <ControlLabel>对手名称</ControlLabel>
        < Select name="Yep" value={this.state.selectedOppoValue} options={this.state.remainingPlayers} onChange={this.onSelectOppo.bind(this)} clearable={false}/>
        < PlayerDetail cardProps={this.state.cards} playerProps={this.state.selectedOppo}/>
      </Col>
      <Col className="container-fluid" md={3}>
      <div><b>开打？</b></div>
      <Button bsStyle="primary" bsSize="large" onClick={this.beginBattle.bind(this)}>开打！</Button>
      </Col>
      </div>
    )

  }
}

export default App;
