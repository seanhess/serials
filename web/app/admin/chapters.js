
// @flow

import React from 'react'

export class Chapters extends React.Component {
  render() {
    var chapters = this.props.chapters || []

    function row(chapter) {
      return <li>
        <a href={chapter.chapterURL}>
          {chapter.chapterName}
        </a>
      </li>
    }

    return <div>
      <ul>
        {chapters.map(row)}
      </ul>
    </div>
  }
}
