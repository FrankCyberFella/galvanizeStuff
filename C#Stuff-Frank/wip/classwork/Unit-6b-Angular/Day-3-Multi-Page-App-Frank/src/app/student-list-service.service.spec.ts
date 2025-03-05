import { TestBed } from '@angular/core/testing';

import { StudentListServiceService } from './student-list-service.service';

describe('StudentListServiceService', () => {
  let service: StudentListServiceService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(StudentListServiceService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
