import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import 'whatwg-fetch';

import Form from "react-jsonschema-form";

import Select from 'react-select';
import 'react-select/dist/react-select.css';

import {Col, FormControl, FormGroup, ControlLabel} from "react-bootstrap";

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


class PropField extends Component {
  constructor(props) {
    super(props);

    this.names = {
        id : "ID",
        player_name : "玩家姓名",
        image_name : "头像图片名",
        association : "公会",
        expi : "经验",
        level : "等级",
        coins : "金币",
        diamonds : "钻石",
        preset_card_id : "预设卡牌",
        selected_skills : "已选技能",
        rank : "排名",

        card_id : "预设卡ID",
        card_name : "卡牌名称",
        card_image_name : "卡牌头像图片名",
        card_level: "卡牌等级",
        card_expi : "卡牌经验值",
        hp : "卡牌血量",
        range_type : "攻击类型",
        class : "职业"
    }

    this.skillNames = {
      brave_shield_counterback: "英勇盾击",
      blade_dance:        "刀剑乱舞",
      assault:          "猎人大招",
      freeze:           "法师大招",
      single_attack:        "单次攻击",
      double_attack:        "二连击",
      triple_attack:        "三连击",
      charm_of_foresight:     "预判",
      fortify_armor:        "闪避增强",
      increase_crit:        "暴击增强",
      counterattack:        "反击",
      concussion:         "冲击",
      talisman_of_shielding:    "护盾",
      deadly_strike:        "死亡一击",
      critical_strike:      "暴击",
      shield_breaker:       "破盾",
      unbalancing_strike:     "失去平衡",
      talisman_of_death:      "致死",
      rune_of_the_void:     "虚无",
      talisman_of_spellshrouding: "抗魔",
      holy_hand_grenade:      "人品手雷",
      poison_gas:         "迷烟",
      shield_wall:        "盾墙",
      sure_hit:         "必中",
      double_swing:       "双重攻击",
      chain_lock:         "武器链",
      first_aid:          "急救",
      healing_potion:       "恢复药剂",
      pierce_armor:       "破甲",
      flurry:           "瞬步",
      spellbreak:         "扰乱",
      perfect_strike:       "精准刺杀",
      vampiric_bolt:        "吸血",
      arcane_surge:       "秘法",
      lower_resist:       "抗性降低",
      pyromania:          "点燃",
      mind_blast:         "精神干扰",
      tornado:          "龙卷风",
      mend:           "修补",
      outbreak:         "瘟疫蔓延",
      roots:            "缠绕",
      tree_hide:          "树皮护甲",
      attack_prim:        "主手攻",
      attack_secd:        "副手攻",
      critical_prim:        "主手暴",
      critical_secd:        "副手攻",
      dodge_prim:         "主手闪避",
      dodge_secd:         "副手闪避",
      block_prim:         "主手格挡",
      block_secd:         "副手格挡",
      resist_prim:        "主手抵抗",
      resist_secd:        "副手抵抗",
      none:           "空技能"
    }
  }

  field(name, val){

    let actualVal = {label:this.skillNames[val], value:val}

    switch(name){
    case "selected_skills" :
      return (< Select
        value={this.props.selectedVal}
        options={Object.keys(this.skillNames).map(skillName => ({label:this.skillNames[skillName], value:skillName}))}
        onChange={this.props.onSelectChange}/>
      );
    default :
      return (<FormControl type="text" value={val} placeholder="Whatever" onChange={this.props.onChange} />)
    }
  }

  render() {
    // console.log(this.props)
    return (
      <div style={{width:"20%", float:"left", marginRight:"1em"}}>
      <ControlLabel>{this.names[this.props.name]}</ControlLabel>
      {this.field(this.props.name, this.props.value)}
      </div>
    );
  }
}

class PlayerDetail extends Component {
  constructor(props) {
    super(props);
  }


  createSingleProp(i, k, v){
    return (<PropField key={i} name={k} value={v} onSelectChange/>)
  }

  createProps(obj){
    return Object.keys(obj).map((key, index) => this.createSingleProp(index, key, obj[key]));
  }

  render() {
    console.log(this.props)
    return (
      <div>
        {this.createProps(this.props.playerProps)}
      </div>
    );
  }
}


class Body extends Component {

  constructor(){
    super()
    this.state = {
      playerList:[],
      options:[],

      selectedSelf:{},
      selectedSelfValue:{},
      selectedOppo:{}
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
      console.log(list)
      this.setState({playerList:list, options:options})
    })
    .catch(error => {
      console.log(error);
      return error
    })
  }

  /**
   * 更新选择的玩家
   * @param  {[type]} selected [description]
   * @return {[type]}          [description]
   */
  updateSelected(selected){
    let playerData = (this.state.playerList.filter(elem => elem.id === selected.value))[0];
    this.setState({selectedSelf:playerData, selectedSelfValue:selected})
  }

  // 开始加载元素之前获得玩家列表
  componentWillMount(){
    this.getPlayerList()
  }


  render() {

    return (

      <div className="container-fluid">
        < Select name="Yep" value={this.state.selectedSelfValue} options={this.state.options} onChange={this.updateSelected.bind(this)}/>
        < PlayerDetail playerProps={this.state.selectedSelf}/>
      </div>
    )

  }
}

export default App;
