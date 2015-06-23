// @flow

import React from 'react'
import {findAll, Submission} from '../model/submission'
import {assign} from 'lodash'
import {CollapseBorder, Cell} from '../style'
import {SourceCover, Cover, CoverOverlay, CoverThumb} from'../cover'

export class Submissions extends React.Component {

  props: {
    submissions: Array<Submission>;
  };

  static load() {
    return {submissions: findAll()}
  }

  render():React.Element {
    var subs = this.props.submissions || []
    return <div>
      <h3>Submissions</h3>
      <div>{subs.map(s => <SubRow submission={s} />)}</div>
    </div>
  }
}

export class SubRow extends React.Component {

  props: {
    submission: Submission;
  };

  render():React.Element {
    var submission = this.props.submission
    return <div className="row" style={Cell}>
      <div style={{display: 'table-cell', verticalAlign: 'top', paddingRight: 10}}>
        <Cover src={submission.imageUrl} size={CoverThumb} />
      </div>
      <div style={{display: 'table-cell'}}>
        <div style={{fontWeight: 'bold'}}>{submission.title}</div>
        <div>{submission.author}</div>
      </div>
    </div>
  }
}
