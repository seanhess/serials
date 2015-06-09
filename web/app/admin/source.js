// @flow

import React from 'react'
import Router from 'react-router'

import {SourceModel, emptySource, emptyScan, Status} from '../model/source'
import {ChapterModel, Chapter, emptyChapter, emptyTitle} from '../model/chapter'
import {Alerts} from '../model/alert'
import {toDateString} from '../helpers'
import {Chapters} from './chapters'
import {ImportSettings} from './import'
import {DisabledButton, FormSection} from '../comp'

import {coverStyle, SourceCover} from '../cover'
import {makeUpdate, checked} from '../data/update'
import {displayIf} from '../style'
import {transitionTo} from '../router'

export class Source extends React.Component {

  static load(params) {
    if (params.id == "new") {
      return {source: emptySource(), chapters:[]}
    }

    return {
      source: SourceModel.find(params.id),
      chapters: ChapterModel.findBySource(params.id),
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {source: emptySource(), scanning: false, chapters: []}
  }

  componentWillReceiveProps(props:any) {
    this.setState({
      // store them locally so you can refresh them!
      source: props.source || emptySource(),
      chapters: props.chapters || []
    })
  }


  onSaveClick() {
    if (this.isNew()) {
      return this.create()
    }

    else {
      return this.save()
    }
  }

  isNew():boolean {
    return this.props.params.id == "new"
  }

  onSaveChapter(chapter:Chapter) {
    ChapterModel.save(chapter)
    .then(this.reloadChapters.bind(this))
  }

  reloadChapters() {
    return ChapterModel.findBySource(this.props.params.id)
    .then((chapters) => {
      this.setState({chapters: chapters})
    })
  }

  onClearChapter(chapter:Chapter) {
    ChapterModel.clear(chapter)
    .then(this.reloadChapters.bind(this))
  }

  onDeleteChapter(chapter:Chapter) {
    ChapterModel.delete(chapter.id)
    .then(this.reloadChapters.bind(this))
  }

  onHiddenChapter(chapter:Chapter, hidden:boolean) {
    ChapterModel.hidden(chapter, hidden)
    .then(this.reloadChapters.bind(this))
  }

  save() {
    var source = this.state.source
    return SourceModel.save(this.props.source.id, source)
    .then(() => Alerts.update("success", "Saved!"))
  }

  create() {
    var source = this.state.source
    return SourceModel.create(source)
    .then(() => window.location.hash = "/admin/sources")
  }

  runScan() {
    if (this.isNew()) {
      throw new Error("Cannot scan new source")
    }

    // save first
    this.save()
    .then(() => this.setState({scanning: true}))
    .then(() => ChapterModel.importSource(this.props.params.id))
    .then(() => this.setState({scanning: false}))
    .then(this.reloadChapters.bind(this))
    .then(() => Alerts.update("success", "Scan complete"))
  }

  deleteAllChapters() {
    ChapterModel.deleteBySource(this.props.params.id)
    .then(this.reloadChapters.bind(this))
  }

  onUpdateSettings(settings:ImportSettings) {
    var source = this.state.source
    source.importSettings = settings
    this.setState({source: source})
  }

  addNewLink() {
    var chapter = emptyChapter(this.state.source.id)
    ChapterModel.save(chapter).then(this.reloadChapters.bind(this))
  }

  addNewTitle() {
    var chapter = emptyChapter(this.state.source.id, emptyTitle())
    ChapterModel.save(chapter).then(this.reloadChapters.bind(this))
  }

  render():?React.Element {
    var source:Source = this.state.source || {}
    var chapters = this.state.chapters || []
    var lastScan = source.lastScan || emptyScan()

    var scanningDisabled = (this.state.scanning) ? "disabled" : ""
    var scanningText = (this.state.scanning) ? "Scanning..." : "Scan Now"

    var update = makeUpdate(source, (v) => {
      this.setState({source: v})
    })

    return <div>
      <h3>Source</h3>

      <div>
        <button className="" onClick={this.onSaveClick.bind(this)}>Save</button>
        <span> </span>
        <a className="secondary button" href="#/admin/sources">Close</a>
      </div>

      <FormSection title="Basic Settings">
        <label>Name</label>
        <input type="text"
          value={source.name}
          onChange={update((s, v) => s.name = v)}
        />
        <label>Author</label>
        <input type="text"
          value={source.author}
          onChange={update((s, v) => s.author = v)}
        />

        <label>Author URL</label>
        <input type="text"
          value={source.authorUrl}
          onChange={update((s, v) => s.authorUrl = v)}
        />

        <label>Status</label>
        <select
          value={source.status}
          onChange={update((s, v) => s.status = v)}
        >
          <option value={Status.Active}>{Status.Active}</option>
          <option value={Status.Complete}>{Status.Complete}</option>
          <option value={Status.Disabled}>{Status.Disabled}</option>
          <option value={Status.Abandoned}>{Status.Abandoned}</option>
        </select>
      </FormSection>

      <FormSection title="Image Settings">

        <div>
          <div style={{float: 'left', width: 170}}>
            <SourceCover source={source} />
          </div>

          <div style={{marginLeft: 170, minHeight: 250}}>
            <label>URL</label>
            <input type="text"
              value={source.url}
              onChange={update((s, v) => s.url = v)}
            />
            <label>Image URL</label>
            <input type="text"
              value={source.imageUrl}
              onChange={update((s, v) => s.imageUrl = v)}
            />

            <label>Artist</label>
            <input type="text"
              value={source.imageArtist}
              onChange={update((s, v) => s.imageArtist = v)}
            />

            <label>Artist URL</label>
            <input type="text"
              value={source.imageArtistUrl}
              onChange={update((s, v) => s.imageArtistUrl = v)}
            />

            <div>
              <label>Image Missing Title</label>
              <input type="checkbox"
                checked={source.imageMissingTitle}
                onChange={update((s, v) => s.imageMissingTitle = v, checked)}
              />
            </div>
          </div>
        </div>

      </FormSection>

      <FormSection title="Import Settings">
        <ImportSettings settings={source.importSettings} onUpdate={this.onUpdateSettings.bind(this)} />
      </FormSection>


      <div style={displayIf(!this.isNew())}>
        <h4>{chapters.length} Chapters</h4>

        <div>
          Last Scan
          <ul>
            <li>Date: {toDateString(lastScan.date)}</li>
            <li>Total: {lastScan.total}</li>
            <li>New: {lastScan.new.length}</li>
            <li>Updated: {lastScan.updated.length}</li>
          </ul>
        </div>

        <div className="right">
          <button className="secondary" onClick={this.addNewTitle.bind(this)}>Add Title</button>
          <span> </span>
          <button className="secondary" onClick={this.addNewLink.bind(this)}>Add Chapter</button>
        </div>

        <div className="">
          <button className={scanningDisabled} onClick={this.runScan.bind(this)}>{scanningText}</button>
          <span> </span>
          <button className="secondary" onClick={this.deleteAllChapters.bind(this)}>Delete All</button>
        </div>


        <Chapters chapters={chapters} source={source}
          onSaveChapter={this.onSaveChapter.bind(this)}
          onClearChapter={this.onClearChapter.bind(this)}
          onDeleteChapter={this.onDeleteChapter.bind(this)}
          onHiddenChange={this.onHiddenChapter.bind(this)}
        />
      </div>

    </div>

  }
}
