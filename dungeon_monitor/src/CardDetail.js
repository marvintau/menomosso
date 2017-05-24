import React, { Component } from 'react';

import Select from 'react-select';
import 'react-select/dist/react-select.css';

import {PropField} from "./PropField.js";
import {Button} from "react-bootstrap";

class CardDetail extends Component{
  constructor(props) {
    super(props);

    this.state = {
    	schema : {
			id  : {type:"string", title:"卡牌ID", display:true, readonly:true},
			card_name  : {type:"string", title:"卡牌名称", display:true},
			image_name  : {type:"string", title:"头像名称", display:true},
			profession  : {type:"select", title:"职业", options:[{value:"hunter", label:"猎人"}, {value:"warrior", label:"战士"}, {value:"rogue", label:"刺客"}, {value:"mage", label:"法师"}],display:true},
			atk_type  : {type:"select", title:"类型", options: [{value:"magic", label:"魔法"}, {value:"physical", label:"物理"}], display:true},
			range_type  : {type:"select", title:"范围", options:[{value:"near", label:"近战"}, {value:"far", label:"远战"}], display:true},
			expi  : {type:"string", title:"经验", display:true},
			stars  : {type:"string", title:"星儿", display:true},
			level  : {type:"string", title:"等级", display:true},
			hp  : {type:"string", title:"血量", display:true},
			armor  : {type:"string", title:"护甲", display:true},
    		agility  : {type:"string", title:"敏捷", display:true},
			atk_min  : {type:"string", title:"最小攻击", display:true},
			atk_max  : {type:"string", title:"最大攻击", display:true},
			block  : {type:"string", title:"格挡", display:true},
			critical  : {type:"string", title:"暴击", display:true},
			dodge  : {type:"string", title:"闪避", display:true},
			hit  : {type:"string", title:"命中", display:true},
			resist  : {type:"string", title:"抵抗", display:true}
    	},
    	card: this.props.cardProps
    }

  }

  componentWillMount(){
  	this.setState({card:this.props.cardProps});
  }

  createSingleProp(prop, index){
    return (<PropField style={{width:"20%"}} key={index} ref={prop} schema={this.state.schema[prop]} value={this.state.card[prop]}/>)
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


  createSelect(){
	let cards = this.props.cardProps.map(card=>({label:card.card_name, value:card.id}));
	let selected = cards.filter(card => (card.value === this.props.player.preset_card_id))[0]

	return (< Select value={selected} options={cards} onChange={this.props.onChange} />);  		
  }

  componentWillReceiveProps(props){
  	if(props.cardProps.length>0){
  		console.log("set props")
  		this.setState({card : props.cardProps.filter(card => card.id === props.player.preset_card_id)[0]});	
  	}
  }

  handleClick(){

  	let card = {}

  	for (var prop in this.state.schema){
  		if(this.state.schema.hasOwnProperty(prop)){

  			let value = this.refs[prop].refs.prop.props.value;

		  	// when input is a select box
		  	if(value.hasOwnProperty("value")){
		  		console.log(prop + " " + value.value);
		  		card[prop] = value.value.toString();
		  	} else {
		  		console.log(prop + " " + value);
		  		card[prop] = value.toString();
		  	}
  		}
  	}

  	this.props.afterGetCardProps(card);
  }

  render(){
  	if(this.props.cardProps.length>0){
	  	return (
			<div>
			{this.createSelect()}
			{this.createProps()}
	        <hr/>
	        <Button bsStyle="success" bsSize="sm" onClick={this.handleClick.bind(this)}>修改卡牌属性并提交</Button>

			</div>
	  	)  		
  	} else {
  		return (<div/>)
  	}
  }
}

export {CardDetail}