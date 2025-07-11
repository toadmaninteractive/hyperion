import { AfterViewInit, ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { animate, state, style, transition, trigger } from '@angular/animations';
import { ActivatedRoute } from '@angular/router';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { BehaviorSubject, combineLatest, merge, Observable, Subject } from 'rxjs';
import { map, startWith, switchMap, take, takeUntil } from 'rxjs/operators';
import { Lightbox } from 'ngx-lightbox';
import { HyperionAttachmentService } from '../../../protocol/attachment-protocol.service';
import { HyperionSetupService } from '../../../protocol/setup-protocol.service';
import { HyperionTestCaseService } from '../../../protocol/test-case-protocol.service';
import { HyperionTestRunService } from '../../../protocol/test-run-protocol.service';
import { AttachmentOwner, FileAttachment, Project, SetupStep, TestCase, TestRun, TestRunItem } from '../../../protocol/db-protocol.data';
import { FileAttachmentOrderBy } from '../../../protocol/attachment-protocol.data';
import { CollectionSlice, OrderDirection } from '../../../protocol/data-protocol.data';


@Component({
    selector: 'm-attachments',
    templateUrl: './attachments.component.html',
    styleUrls: ['./attachments.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
    animations: [
        trigger('detailExpand', [
            state('collapsed', style({height: '0px', minHeight: '0'})),
            state('expanded', style({height: '*'})),
            transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
        ]),
    ],
})
export class AttachmentsComponent implements OnInit, OnDestroy, AfterViewInit {
    @ViewChild(MatPaginator) paginator: MatPaginator;
    @ViewChild(MatSort) sort: MatSort;

    destroy$: Subject<any>;

    project: Project;
    displayedColumns: string[] = ['filename', 'filesize', 'content_type', 'created',  'actions'];
    data: FileAttachment[] = [];
    expandedRow: FileAttachment | null;
    detailInfoSetupStep = [];
    detailInfoTestCase = [];
    detailInfoTestRun = [];
    detailInfoTestRunItem = [];
    resultsLength = 0;
    isLoadingResults = true;
    firstLoad = true;
    filterValue = '';

    constructor (
        private route: ActivatedRoute,
        private cdr: ChangeDetectorRef,
        private lightbox: Lightbox,
        private hyperionAttachmentService: HyperionAttachmentService,
        private hyperionSetupService: HyperionSetupService,
        private hyperionTestCaseService: HyperionTestCaseService,
        private hyperionTestRunService: HyperionTestRunService,
    ) { }

    ngOnInit(): void {
        this.destroy$ = new Subject();

        this.route.data
            .pipe(
                takeUntil(this.destroy$),
            )
            .subscribe(response =>  {
                this.data = [];
                this.project = response.activeProject;
                if (!this.firstLoad) {
                    this.refresh();
                }
            });
    }

    ngAfterViewInit(): void {
        this.firstLoad = false;
        this.sort.sortChange.subscribe(() => this.paginator.pageIndex = 0);

        merge(this.sort.sortChange, this.paginator.page)
            .pipe(
                startWith({}),
                switchMap(() => {
                    console.log(this.sort.active, this.sort.direction, this.sort);
                    this.isLoadingResults = true;
                    return this.loadData();
                }),
                map(data => {
                    // Flip flag to show that loading has finished.
                    this.isLoadingResults = false;
                    this.resultsLength = data.total;

                    return data.items;
                })
            ).subscribe(data => {
                this.data = data;
                this.cdr.detectChanges();
        });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    applyFilter(filterValue: string): void {
        this.filterValue = filterValue;
        this.refresh();
    }

    loadData(): Observable<CollectionSlice<FileAttachment>> {

        return this.hyperionAttachmentService
            .getProjectAttachments(
                this.project.id,
                this.getSortField() ? this.getSortField() : FileAttachmentOrderBy.OriginalFilename,
                this.sort.direction === 'asc' ? OrderDirection.Asc : OrderDirection.Desc,
                this.paginator.pageIndex * this.paginator.pageSize,
                this.paginator.pageSize,
                this.filterValue === '' ? null : this.filterValue);
    }

    getSortField(): FileAttachmentOrderBy {
        switch (this.sort.active) {
            case 'id': return FileAttachmentOrderBy.Id;
            case 'filesize': return FileAttachmentOrderBy.FileSize;
            case 'filename': return FileAttachmentOrderBy.OriginalFilename;
            case 'content_type': return FileAttachmentOrderBy.ContentType;
            case 'personnel_id': return FileAttachmentOrderBy.PersonnelId;
            case 'personnel': return FileAttachmentOrderBy.PersonnelName;
            case 'created': return FileAttachmentOrderBy.CreatedAt;
            default: return null;
        }
    }

    requestLinks(attachment: FileAttachment): void {
        this.expandedRow = this.expandedRow === attachment ? null : attachment;
        if (this.expandedRow) {
            this.hyperionAttachmentService
                .getAttachmentLinks(attachment.id)
                .pipe(
                    take(1),
                    map(response => response.items.filter(item => item.projectId === this.project.id)),
                    switchMap(response => {
                            const setupRequests  = response.filter(elem => elem.owner === AttachmentOwner.SetupStep)
                                .map(elem => this.hyperionSetupService.getSetupStep(elem.linkedId));
                            const testCaseRequests  = response.filter(elem => elem.owner === AttachmentOwner.TestCase)
                                .map(elem => this.hyperionTestCaseService.getTestCase(elem.linkedId));
                            const testRunRequests  = response.filter(elem => elem.owner === AttachmentOwner.TestRun)
                                .map(elem => this.hyperionTestRunService.getTestRunItem(elem.linkedId));
                            const testRunItemsRequests  = response.filter(elem => elem.owner === AttachmentOwner.TestRunItem)
                                .map(elem => this.hyperionTestRunService.getTestRunItem(elem.linkedId));
                            return combineLatest(...setupRequests, ...testCaseRequests, ...testRunRequests, ...testRunItemsRequests);
                        }
                    )
                )
                .subscribe((resp: Array<SetupStep | TestCase | TestRun | TestRunItem>) => {
                    this.detailInfoSetupStep = resp.filter(item => item instanceof SetupStep);
                    this.detailInfoTestCase = resp.filter(item => item instanceof TestCase);
                    this.detailInfoTestRun = resp.filter(item => item instanceof TestRun);
                    this.detailInfoTestRunItem = resp.filter(item => item instanceof TestRunItem);
                    this.cdr.detectChanges();
                });
        } else {
            this.detailInfoSetupStep = [];
            this.detailInfoTestCase = [];
            this.detailInfoTestRun = [];
            this.detailInfoTestRunItem = [];
        }

    }

    refresh(): void {
        this.isLoadingResults = true;
        this.loadData()
            .pipe(take(1))
            .subscribe(data => {
                this.isLoadingResults = false;
                this.paginator.pageIndex = 0;
                this.resultsLength = data.total;
                this.data = data.items;
                this.cdr.detectChanges();
            });
    }

    openImage(row: FileAttachment): void {
        this.lightbox.open([{
            src: '/download/attachments/' + row.filename,
            thumb: '/download/attachments/' + row.thumbFilename
        }], 0);
    }
}
