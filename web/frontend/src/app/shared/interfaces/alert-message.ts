export interface AlertMessage {
    message: string;
    kind: 'success' | 'info' | 'warning' | 'danger' | 'brand' | 'primary';
    dismissable: boolean;
}
