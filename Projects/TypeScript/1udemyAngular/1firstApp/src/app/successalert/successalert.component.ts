import { Component } from '@angular/core';

@Component({
  selector: 'app-successalert',     //  as an element, most commonly used
  // selector: '[app-servers]',    as an attribute
  // selector: '.app-servers', //  as a class
  templateUrl: './successalert.component.html',
  // template: `
  //   <app-server></app-server>
  //   <app-server></app-server>`,
  styles: [`
    body {
      color: white;
      padding: 20px;
      background-color: limegreen;
      border: 1px solid green;
    }
  `]
})
export class SuccessAlertComponent {

}
