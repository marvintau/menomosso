import React, { Component } from 'react';

import Select from 'react-select';
import {FormControl, ControlLabel} from "react-bootstrap";

/// 显示一个属性，可能由Select或者Text表示
class PropField extends Component {
  constructor(props) {
    super(props);
    this.state = {}

    this.handleChange = this.handleChange.bind(this)
  }

  handleChange(event){
    console.log(event.target);
    this.setState({value:event.target.value});
  }

  handleSelectChange(selected){
    if(this.props.schema.multi){
      console.log(this.state.value) 
      if(this.props.schema.limit && this.state.value.split(",").length <= this.props.schema.limit){
        let joined = (selected.map(sel => sel.value)).join(",");
        this.setState({value:joined});
      }      
    } else {
      this.setState({value:selected});
    }
  }

  componentWillMount(){

    let value;

    switch(this.props.schema.type){
      case "select":
      
        if(this.props.schema.multi){
          console.log(this.props.value);
          value = this.props.value.join(",");
        } else {
          value = this.props.schema.options.filter(option => (option.value === this.props.value))[0];
        }

      break;
      default:
        value = this.props.value;
    }

    this.setState({value:value})
  }

  field(){

    switch(this.props.schema.type){
    case "select" :

      let filterOptions = (options, searchFilter, selectedOptions) => {
        return options
      }

      return (< Select
        ref="prop"
        value={this.state.value}
        multi={this.props.schema.multi} 
        options={this.props.schema.options}
        onChange={this.handleSelectChange.bind(this)}
        inputProps={{readOnly:this.props.schema.readOnly}}
        filterOptions={filterOptions}/>);
    
    default :

      if(this.props.schema.readonly){
        return (<FormControl ref="prop" readOnly type="text" value={this.state.value} placeholder="Whatever" onChange={this.handleChange} />)
      } else {
        return (<FormControl ref="prop" type="text" value={this.state.value} placeholder="Whatever" onChange={this.handleChange} />)
      }

    }
  }

  componentWillReceiveProps(nextProp){
    this.setState({value:nextProp.value});
  }

  render() {
    return (
      <div>
      <ControlLabel>{this.props.schema.title}</ControlLabel>
      {this.field()}
      </div>
    );
  }
}

export {PropField};