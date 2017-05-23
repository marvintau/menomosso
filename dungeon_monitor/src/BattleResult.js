import React, { Component } from 'react';

class BattleResult extends Component {

	constructor(){
		super();
		this.state = {battleResult : {}, self:{}, oppo:{}}

	}

	parseRecord(record){

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

		console.log(record);
		let selfName = record[this.state.self.id].player_name,
			oppoName = record[this.state.oppo.id].player_name;

		let off, def;
		if(record[this.state.self.id].role === "offender"){
			off = selfName;
			def = oppoName;
		} else {
			off = oppoName;
			def = selfName;
		}

		let role = off;

		let attrs = {
			hp : "生命",
			armor : "护甲",
			hit : "命中概率",
			critical : "暴击概率",
			dodge : "闪避概率",
			block : "格挡概率",
			resist : "抵抗概率"
		}

		let action;

		switch(record.effect.outcome){
			case "successful":
			action = " 使用了 " + skills.filter(skill => skill.value === record.effect.skill_name)[0].label;
			break;

			case "failed":
			action = " 对 " + def + " 施放技能 " + skills.filter(skill => skill.value === record.effect.skill_name)[0].label + " 失败";
			break;
			default:

				let dest = (record.effect.dest === "offender"? off : def);

				switch(record.effect.outcome){
					case "attack":
						action = " 命中了 " + dest + " 产生效果: " + attrs[record.effect.attr] + " " + (record.effect.diff > 0 ? "增加" : "减少") + Math.abs(record.effect.diff);
						break;
					case  "cast":
						action = " 对 " + dest + " 产生效果: " + attrs[record.effect.attr] +" " + (record.effect.diff > 0 ? "增加" : "减少") + Math.abs(record.effect.diff);
						break;
					case "critical":
						action = " 暴击了 " + dest + " ，产生效果: " + attrs[record.effect.attr] + " " + (record.effect.diff > 0 ? "增加" : "减少") + Math.abs(record.effect.diff);
						break;
					case "dodge":
						action = " 的攻击被 " + dest + " 闪避了";
						break;
					case "block":
						action = " 的攻击被 " + dest + " 格挡了";
						break;
					case "resist":
						action = " 的攻击被 " + dest + " 抵抗了";
						break;
				}

			break;
		}

	
		return role + action;
	}

	showResult(){
		let round = 0;
		return this.state.battleResult.records.map((record, i) => {

			let roundText = (record.state.seq !== round) ? (<div className="round">{"第 " + record.state.seq + " 回合"}</div>) : (<div/>)
			round = record.state.seq;

			return (
				<div key={i}>
					{roundText}
					{i +":\t" + this.parseRecord(record)}
				</div>
			)
		});
	}

	render(){
		if(this.state.battleResult.hasOwnProperty("records")){
			return(<div  className="result">{this.showResult()}</div>)	
		} else {
			return(<div/>)
		}
		
	}

	componentWillReceiveProps(props){
		this.setState({
			battleResult : props.battleResult,
			self : props.self,
			oppo : props.oppo
		})
	}
}

export {BattleResult};