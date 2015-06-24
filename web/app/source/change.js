// @flow

import React from 'react'
import {findChange, Source} from '../model/source'
import {FormSection} from '../comp'
import {diff} from '../model/change'

//class ChangeLink extends React.Component {
  //render():React.Element {
    //return <Link to={Routes.change} params={this.props.change}>
      //{this.props.children}
    //</Link>
  //}
//}

// render the source form
export class SourceChange extends React.Component {

  static load(params) {
    return {change: findChange("fixme", params.id)}
  }

  render():React.Element {
    var change = this.props.change || {}
    var source = change.source || {}

    function update() {}

    return <div>
      <BookDetails source={source} />
    </div>
  }
}

// identify the changed fields, and print them out
// maybe it shouldn't be so hard?
export class BookDetails extends React.Component {

  props: {
    source: Source
  };

  render():React.Element {
    var source = this.props.source
    // with the previous version. Needs some concept of what it was based on
    return <div>
      <label>Title</label>
      <div>{source.name}</div>
    </div>
  }
}
