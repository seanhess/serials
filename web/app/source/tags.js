// @flow

import React from 'react'
import Select from 'react-select'
import {words, uniq} from 'lodash'
import {TagCount} from '../model/source'

(require : any)('react-select/dist/default.css')

// I need to load the available tags

export class TagsInput extends React.Component {

  props: {
    value: Array<string>;
    onChange: Function;
    tags: Array<TagCount>;
    placeholder: ?string;
  };

  onChange(value:string) {
    if (!this.props.onChange) return
    var tags = uniq(value.toLowerCase().split(DELIM))
    this.props.onChange(tags)
  }

  render():React.Element {

    var value = this.props.value || []
    var tags = this.props.tags || []
    var options = tags.map(tagToOption)

    return <Select
      multi={true}
      placeholder={this.props.placeholder}
      allowCreate={true}
      delimiter={DELIM}
      name="tags-input"
      value={value.join(DELIM)}
      options={options} onChange={this.onChange.bind(this)}
    />
  }
}

export class TagSelect extends React.Component {

  onChange(value:string) {
    if (!this.props.onChange) return
    this.props.onChange(value)
  }

  render():React.Element {
    var value = this.props.value || ""
    var tags = this.props.tags || []
    var options = tags.map(tagToOption)
    return <Select
      placeholder="All Tags"
      name="tag-select"
      value={value}
      options={options}
      onChange={this.onChange.bind(this)}
    />
  }
}

var DELIM = ","

// these clash with the added names, making them disappear :(
// so I need a value that isn't exactly equal to the label
function tagToOption(tc:TagCount):{label:string; value:string} {
  var tl = toTagLabel(tc.tagName)
  return {
    value: tl,
    label: tl + " - " + tc.tagCount
  }
}

function toTagLabel(tag:string):string {
  return tag.split(" ").map(capitalizeFirstLetter).join(" ")
}

function capitalizeFirstLetter(str:string):string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}
