// @flow
import React from 'react'
import {Link} from 'react-router'
import {Header, MainContainer} from './main'

export class NotFound extends React.Component {
  render():React.Element {
    return <div>
      <Header />
      <MainContainer>
        <p style={{marginTop: 10}}>Page not found</p>
        <div><Link to="books">Home</Link></div>
      </MainContainer>
    </div>
  }
}
