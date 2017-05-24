import React, { Component } from 'react';

import 'react-select/dist/react-select.css';

import {PropField} from "./PropField.js";


// 显示一个玩家的信息
class PlayerDetail extends Component {
  constructor(props) {
    super(props);
    
    let skills = [
      {value: "brave_shield_counterback",    label:"英勇盾击"},
      {value: "blade_dance",                 label:"刀剑乱舞"},
      {value: "assault",                     label:"猎人大招"},
      {value: "freeze",                      label:"法师大招"},
      {value: "single_attack",               label:"单次攻击"},
      {value: "double_attack",               label:"二连击"},
      {value: "triple_attack",               label:"三连击"},
      {value: "charm_of_foresight",          label:"预判"},
      {value: "fortify_armor",               label:"闪避增强"},
      {value: "increase_crit",               label:"暴击增强"},
      {value: "counterattack",               label:"反击"},
      {value: "concussion",                  label:"冲击"},
      {value: "talisman_of_shielding",       label:"护盾"},
      {value: "deadly_strike",               label:"死亡一击"},
      {value: "critical_strike",             label:"暴击"},
      {value: "shield_breaker",              label:"破盾"},
      {value: "unbalancing_strike",          label:"失去平衡"},
      {value: "talisman_of_death",           label:"致死"},
      {value: "rune_of_the_void",            label:"虚无"},
      {value: "talisman_of_spellshrouding",  label:"抗魔"},
      {value: "holy_hand_grenade",           label:"人品手雷"},
      {value: "poison_gas",                  label:"迷烟"},
      {value: "shield_wall",                 label:"盾墙"},
      {value: "sure_hit",                    label:"必中"},
      {value: "double_swing",                label:"双重攻击"},
      {value: "chain_lock",                  label:"武器链"},
      {value: "first_aid",                   label:"急救"},
      {value: "healing_potion",              label:"恢复药剂"},
      {value: "pierce_armor",                label:"破甲"},
      {value: "flurry",                      label:"瞬步"},
      {value: "spellbreak",                  label:"扰乱"},
      {value: "perfect_strike",              label:"精准刺杀"},
      {value: "vampiric_bolt",               label:"吸血"},
      {value: "arcane_surge",                label:"秘法"},
      {value: "lower_resist",                label:"抗性降低"},
      {value: "pyromania",                   label:"点燃"},
      {value: "mind_blast",                  label:"精神干扰"},
      {value: "tornado",                     label:"龙卷风"},
      {value: "mend",                        label:"修补"},
      {value: "outbreak",                    label:"瘟疫蔓延"},
      {value: "roots",                       label:"缠绕"},
      {value: "tree_hide",                   label:"树皮护甲"},
      {value: "attack_prim",                 label:"主手攻"},
      {value: "attack_secd",                 label:"副手攻"},
      {value: "critical_prim",               label:"主手暴"},
      {value: "critical_secd",               label:"副手攻"},
      {value: "dodge_prim",                  label:"主手闪避"},
      {value: "dodge_secd",                  label:"副手闪避"},
      {value: "block_prim",                  label:"主手格挡"},
      {value: "block_secd",                  label:"副手格挡"},
      {value: "resist_prim",                 label:"主手抵抗"},
      {value: "resist_secd",                 label:"副手抵抗"},
      {value: "none",                        label:"空技能"}
    ];

    this.state = {

      schema : {
        id :              { title: "玩家ID", type: "string", display: true, readonly:true}, 
        player_name :     { title: "玩家姓名", type: "string", display: true}, 
        h1:               {type: "divider"},
        image_name :      { title: "头像图片名", type: "string", display: false}, 
        association :     { title: "公会", type: "string", display: false}, 
        expi :            { title: "经验", type: "number", display: false}, 
        level :           { title: "等级", type: "number", display: false}, 
        coins :           { title: "金币", type: "number", display: false}, 
        diamonds :        { title: "钻石", type: "number", display: false}, 
        // preset_card_id :  { title: "预设卡牌", type: "select", display: true, options:this.props.cardOptions, multi:false}, 
        rank :            { title: "排名", type: "number", display: false}, 
        selected_skills : { title: "已选技能", type: "select", display: true, options: skills, multi: true, limit:10, readonly:this.props.skillReadOnly}
      },

      playerProps : {}
    }
  }

  createSingleProp(prop, index){
    return (<PropField key={index} ref={prop} schema={this.state.schema[prop]} value={this.state.playerProps[prop]}/>)
  }

  createProps(){
    return Object.keys(this.state.schema).map((prop, index) => {
      if(this.state.schema[prop].display){
        return this.createSingleProp(prop, index);
      } else if (this.state.schema[prop].type === "divider"){
        return (<hr key={index} />);
      } else {
        return (<div key={index}></div>);
      }
    })
  }

  componentWillReceiveProps(props){
    if(props.playerProps.hasOwnProperty("id")){
      // console.log("current: "+this.state.playerProps.player_name)
      // console.log("change:" + props.playerProps.player_name)
      this.setState({playerProps : props.playerProps}, () => {
        // console.log("changed"+props.playerProps.player_name)
      });
    }
  }

  render() {

    if(this.props.playerProps.hasOwnProperty("id")){
      return ( <div> {this.createProps()} </div> );
    } else {
      return ( <div/>);
    }
  }

  componentDidUpdated(){
    console.log(this.state.playerProps.player_name)
  }
}

export {PlayerDetail};