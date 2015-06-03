// @flow
// Google Analytics
// https://developers.google.com/analytics/devguides/collection/analyticsjs/single-page-applications
declare var ga:(event:string, other:string) => void;
import {isProduction} from './settings'

export function pageview(url:string) {
  // ignore if local
  if (isProduction()) {
    console.log("PAGE VIEW", url)
    ga('send', 'pageview', url)
  }
}

// ga('send', 'pageview', '/new-page');
// ga('set', 'page', '/new-page');

//function analytics(state) {
  //ga('send', 'pageview', {
    //'page': state.path
  //});
//}

//ga('set', {
  //page: '/new-page',
  //title: 'New Page'
//});

// ga('send', 'pageview');



//module.exports = analytics;
