
// @flow

import React from 'react'

export class Chapters extends React.Component {
  render() {
    var chapters = this.props.chapters || []

    function row(chapter) {
      return <a href={chapter.chapterURL}>
        {chapter.chapterName}
      </a>
    }

    return <div>
      {chapters.map(row)}
    </div>
  }
}
