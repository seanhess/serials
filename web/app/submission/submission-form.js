// @flow

import React from 'react'
import {Link} from 'react-router'

import {Submission, emptySubmission, findById, create, save, validate} from '../model/submission'
import {defaultImageUrl} from '../model/source'
import {makeUpdate, checked} from '../data/update'
import {DisabledButton, FormSection} from '../comp'
import {coverStyle, Cover, CoverOverlay} from '../cover'
import {transitionTo, Routes} from '../router'
import {Alerts} from '../model/alert'

export class SubmissionForm extends React.Component {

  props: {
    submission: ?Submission;
    params: {id: string};
  };

  static load(params) {
    if (params.id == "new") {
      return {submission: emptySubmission()}
    }

    return {
      submission: findById(params.id),
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {submission: emptySubmission()}
  }

  componentWillReceiveProps(props:any) {
    this.setState({
      submission: props.submission || emptySubmission(),
    })
  }

  isNew():boolean {
    return this.props.params.id == "new"
  }

  onSaveClick():void {
    var sub = this.state.submission

    var result = validate(sub)
    if (result) {
      Alerts.update("error", result)
      return
    }

    var runSave;
    if (this.isNew()) {
      runSave = sub => create(sub)
    }

    else {
      runSave = sub => save(this.props.params.id, sub)
    }

    return runSave(sub)
    .then(() => transitionTo(Routes.submissions))
    .then(() => Alerts.update("success", "Submitted!"))
  }

  render():React.Element {
    var submission:Submission = this.state.submission || {}

    var update = makeUpdate(submission, (v) => {
      this.setState({submission: v})
    })

    return <div>
      <h3>Submit a book</h3>

      <div>
        <button className="" onClick={this.onSaveClick.bind(this)}>Submit</button>
      </div>

      <FormSection title="Book Details">
        <label>Title</label>
        <input type="text"
          placeholder="Harry Potter and the Methods of Rationality"
          value={submission.title}
          onChange={update((s, v) => s.title = v)}
        />

        <label>Table of Contents URL</label>
        <input type="text"
          placeholder="http://hpmor.com"
          value={submission.url}
          onChange={update((s, v) => s.url = v)}
        />

        <label>Author</label>
        <input type="text"
          placeholder="Eliezer Yudkowsky"
          value={submission.author}
          onChange={update((s, v) => s.author = v)}
        />
      </FormSection>

      <FormSection title="Image Details">

        <div>
          <div style={{float: 'left', width: 170}}>
            <Cover src={submission.imageUrl}>
              <CoverOverlay>{submission.title}</CoverOverlay>
            </Cover>
          </div>

          <div style={{marginLeft: 170, minHeight: 250}}>
            <label>Image URL</label>
            <input type="text"
              value={submission.imageUrl}
              onChange={update(function(s, v) {
                s.imageUrl = defaultImageUrl(v)
              })}
            />

            <label>Artist</label>
            <input type="text"
              placeholder="Laniessa"
              value={submission.imageArtist}
              onChange={update((s, v) => s.imageArtist = v)}
            />

            <label>Artist URL</label>
            <input type="text"
              placeholder="http://laniessa.deviantart.com/art/Mr-Mage-in-Training-384580223"
              value={submission.imageArtistUrl}
              onChange={update((s, v) => s.imageArtistUrl = v)}
            />
          </div>
        </div>
      </FormSection>
    </div>

  }
}

