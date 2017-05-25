import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import 'whatwg-fetch';

import {PlayerDetail} from './PlayerDetail.js';
import {CardDetail} from './CardDetail.js';
import {BattleResult} from "./BattleResult.js";

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
      cardList:[],
      cardOptions:[],

      selectedSelf:{},
      selectedOppo:{},
      selectedSelfValue:{},
      selectedOppoValue:{},

      battle_res:[]
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

  getCardList(){
    fetch('http://everstream.cn:1337/api/get_card_list', {
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
      let options = list.map(elem => ({value:elem.id, label:elem.card_name}))
      this.setState({cardList:list, cardOptions:options}, ()=>{
        console.log(this.state);
      })
    })
    .catch(error => {
      console.log(error);
      return error
    })    
  }

  beginBattle(){
    fetch('http://everstream.cn:1337/api/battle_request', {
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
      this.setState({battle_res:res})
    })
    .catch(error => {
      console.log(error);
      return error
    })
  }

  updatePlayerInfo(){

  }


  onSelectSelf(selected){
    console.log("changed self player: "+selected.label);
    // this.getPlayerProfile(selected)
    this.setState((prevState) => {
      prevState.selectedSelfValue = selected;
      prevState.selectedSelf = prevState.playerList.filter(player_profile => player_profile.id === prevState.selectedSelfValue.value)[0]
      prevState.remainingPlayers = prevState.players.filter(playerOption => playerOption.value !== selected.value);

      prevState.selectedOppoValue = prevState.remainingPlayers[0];
      prevState.selectedOppo = prevState.playerList.filter(player_profile => player_profile.id === prevState.selectedOppoValue.value)[0]
      return prevState
    })
  }

  onSelectOppo(selected){
    console.log("changed oppo: "+selected.label);
    this.setState((prevState) => {
      prevState.selectedOppoValue = selected;
      prevState.selectedOppo = prevState.playerList.filter(player_profile => player_profile.id === prevState.selectedOppoValue.value)[0]
      return prevState
    })
  }

  // 开始加载元素之前获得玩家列表
  componentWillMount(){
    this.getCardList()
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

  updateCardInfo(cardProps){
    console.log(cardProps);
    fetch('http://everstream.cn:1337/api/update_card', {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Origin': 'http://everstream.cn:3000',
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'X-Requested-With'
      },
      body: JSON.stringify({
        id: this.state.selectedSelf.preset_card_id,
        updated_card:cardProps
      })
    })
    .then(response => response.json())
    .then(res =>{
      if(res.res === "card_updated"){
        this.getCardList()
      }
    })
    .catch(error => {
      console.log(error);
      return error
    })
  }

  updatePresetSkill(){
    let value = this.refs.self.refs.selected_skills.state.value.split(",");
    this.setState((prevState, _) => {
      prevState.selectedSelf.selected_skills = value;
    })
  }

  render() {

    return (
      <div>
      <Col className="container-fluid" md={3}>
        <ControlLabel>选择对手</ControlLabel>
        < Select name="Yep" value={this.state.selectedOppoValue} options={this.state.remainingPlayers} onChange={this.onSelectOppo.bind(this)} clearable={false}/>
        < PlayerDetail cardProps={this.state.cards} playerProps={this.state.selectedOppo} skillReadOnly={true}/>
        <hr/>
        <ControlLabel>选择己方玩家（第一步）</ControlLabel>
        < Select name="Yep" value={this.state.selectedSelfValue} options={this.state.players} onChange={this.onSelectSelf.bind(this)} clearable={false}/>
        < PlayerDetail ref="self" cardProps={this.state.cards} playerProps={this.state.selectedSelf} skillReadOnly={false}/>
        <hr/><Button bsStyle="success" bsSize="sm" onClick={this.updatePresetSkill.bind(this)}>修改玩家信息并提交</Button>
        <hr/>
        < CardDetail cardProps={this.state.cardList} player={this.state.selectedSelf} onChange={this.onSelectCard.bind(this)} afterGetCardProps={this.updateCardInfo.bind(this)}/>
      </Col>
      <Col className="container-fluid" md={3}>
      <Button bsStyle="primary" bsSize="large" onClick={this.beginBattle.bind(this)}>开打！</Button>
      <BattleResult battleResult={this.state.battle_res} self={this.state.selectedSelf} oppo={this.state.selectedOppo} />
      </Col>
      </div>
    )

  }
}

export default App;
