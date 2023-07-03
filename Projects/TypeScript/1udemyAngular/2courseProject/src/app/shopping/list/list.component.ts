import { Component } from '@angular/core';

import { Ingredient } from '../../shared/ingredient.model'

@Component({
  selector: 'app-list',
  templateUrl: './list.component.html',
  styleUrls: ['./list.component.css']
})
export class ListComponent {
  ingredients: Array<Ingredient> = [
    new Ingredient('Rice',100),
    new Ingredient('Cinnamon',5)
  ];
}
