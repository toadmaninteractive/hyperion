export interface Filter {
    name: string;
    value?: any;
    isRequired?: boolean;
    type?: number;
    selectOptions?: string[];
    isActive?: boolean;
    key?: string;
}
