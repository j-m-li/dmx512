/*++

Copyright (c) 2004 O'ksi'D
Copyright (c) 1997-1998  Microsoft Corporation

Module Name:

   OcrwInt.c

Abstract:

    Generic Bulk and Interrupt USB device driver 
    Read/write io test code

Environment:

    kernel mode only

Notes:

  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
  KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
  PURPOSE.

  Copyright (c) 1997-1998 Microsoft Corporation.  All Rights Reserved.


Revision History:

    11/17/97 : created
    23.08.2004 : (jml@oksid.ch) Interrupt support and OVERLAPPED Read/Write support 
--*/


#include "wdm.h"
#include "stdarg.h"
#include "stdio.h"

#define DRIVER 

#include "usbdi.h"
#include "usbdlib.h"
#include "IntUSB.h"

VOID
OGENINT_CancelWrite(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP           MainIrp
    )
{
	
	PDEVICE_EXTENSION deviceExtension = DeviceObject->DeviceExtension;
	PUCHAR pCon =  (PUCHAR) deviceExtension->PendingIoIrps[0];
	ULONG i = 0;
	PIRP Irp, TheIrp;
	USHORT uDriverCancel = 0;  // count cancelled via iocancelirp()
	BOOLEAN cRes = 0; 
//	NTSTATUS ntStatus
	NTSTATUS waitStatus;

	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("ENTER write cancel io !\n"));

    	IoReleaseCancelSpinLock(MainIrp->CancelIrql);

	TheIrp = deviceExtension->BaseIrp[0];

	if (!TheIrp) return;
	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("GO cancel io !\n"));
	
	// the OGENINT_RW_CONTEXT array is terminated by an entry with a NULL Irp
	for ( i = 0; i < deviceExtension->arraySize[0] / sizeof(OGENINT_RW_CONTEXT) ;  i++ ) {
		Irp = (((POGENINT_RW_CONTEXT) pCon) + i)->Irp;
		//
		// Since IoCallDriver has been called on this request, we call IoCancelIrp
		//  and let our completion routine handle it
		//
		if (Irp) {
			OGENINT_KdPrint ( DBGLVL_DEFAULT, ("GO IOcancel io !\n"));
			cRes = IoCancelIrp( Irp );
			OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OK IOcancel io !\n"));

        		uDriverCancel++; // flag we tried to cancel at least one
		}

	} // end, for


	if (uDriverCancel && cRes ) {

		// We only get here if we cancelled at least one and all cancellations were successfull.
		// Wait on the event set on last cancel in OGENINT_AsyncReadWriteComplete();
	
		waitStatus = KeWaitForSingleObject(
                       &deviceExtension->StagingDoneEvent,
                       Suspended,
                       KernelMode,
                       FALSE,
                       NULL);    
	}

	TheIrp->IoStatus.Information = 0;
	TheIrp->IoStatus.Status = STATUS_CANCELLED;
	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("EXIT write cancel io !\n"));
}

VOID
OGENINT_CancelRead(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP           MainIrp
    )
{
	
	PDEVICE_EXTENSION deviceExtension = DeviceObject->DeviceExtension;
	PUCHAR pCon =  (PUCHAR) deviceExtension->PendingIoIrps[1];
	ULONG i = 0;
	PIRP Irp, TheIrp;
	USHORT uDriverCancel = 0;  // count cancelled via iocancelirp()
	BOOLEAN cRes = 0; 
//	NTSTATUS ntStatus, 
	NTSTATUS waitStatus;

	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("ENTER read cancel io !\n"));

    	IoReleaseCancelSpinLock(MainIrp->CancelIrql);

	TheIrp = deviceExtension->BaseIrp[1];

	if (!TheIrp) return;
	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("GO cancel io !\n"));
	
	// the OGENINT_RW_CONTEXT array is terminated by an entry with a NULL Irp
	for ( i = 0; i < deviceExtension->arraySize[1] / sizeof(OGENINT_RW_CONTEXT) ;  i++ ) {
		Irp = (((POGENINT_RW_CONTEXT) pCon) + i)->Irp;
		//
		// Since IoCallDriver has been called on this request, we call IoCancelIrp
		//  and let our completion routine handle it
		//
		if (Irp) {
			OGENINT_KdPrint ( DBGLVL_DEFAULT, ("GO IOcancel io !\n"));
			cRes = IoCancelIrp( Irp );
			OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OK IOcancel io !\n"));

        		uDriverCancel++; // flag we tried to cancel at least one
		}
	} // end, for


	if (uDriverCancel && cRes ) {
		// We only get here if we cancelled at least one and all cancellations were successfull.
		// Wait on the event set on last cancel in OGENINT_AsyncReadWriteComplete();
	
		waitStatus = KeWaitForSingleObject(
                       &deviceExtension->StagingDoneEvent,
                       Suspended,
                       KernelMode,
                       FALSE,
                       NULL);
	}

	TheIrp->IoStatus.Information = 0;
	TheIrp->IoStatus.Status = STATUS_CANCELLED;
	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("EXIT read cancel io !\n"));
}

void freeMdls(PDEVICE_EXTENSION deviceExtension)
{
	int ok = 0;
	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("enter OGENINT_freeMdls(%d)\n", deviceExtension->NbMdls));

	while (deviceExtension->NbMdls > 0) {
		deviceExtension->NbMdls--;
		ok = 1;
		IoFreeMdl(deviceExtension->Mdls[deviceExtension->NbMdls]);
		deviceExtension->Mdls[deviceExtension->NbMdls] = NULL;
	}
	if (ok) OGENINT_ExFreePool(deviceExtension->Mdls);
}

NTSTATUS
OGENINT_StagedReadWrite(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN BOOLEAN Read
    )
/*++

Routine Description:

    This routine is called by OGENINT_Read() for IRP_MJ_READ.
    This routine is called by OGENINT_Write() for IRP_MJ_WRITE.

    Breaks up a read or write in to specified sized chunks,
    as specified by deviceExtension->MaximumTransferSize

Arguments:

    DeviceObject - pointer to our FDO ( Functional Device Object )

    Irp - pointer to the IRP_MJ_READ or IRP_MJ_WRITE

    Read - TRUE for read, FALSE for write


Return Value:

    NT status code

--*/
{
	NTSTATUS ntStatus = STATUS_SUCCESS;
	NTSTATUS resetPipeStatus;
	PFILE_OBJECT fileObject;
	PIO_STACK_LOCATION irpStack, nextStack;
	PURB urb;
	PIRP irp;
	PMDL mdl; 
	PVOID va;
	CHAR stackSize;
//	KIRQL OldIrql;
//	BOOLEAN fRes;
//	NTSTATUS waitStatus;
	ULONG nIrps = 0, totalLength = 0, totalIrpsNeeded, used;
	PUSBD_PIPE_INFORMATION pipeHandle = NULL;
	PDEVICE_EXTENSION deviceExtension = DeviceObject->DeviceExtension;
	PUCHAR pCon =  NULL;
	ULONG ChunkSize = deviceExtension->MaximumTransferSize;
	ULONG arraySize;
	POGENINT_RW_CONTEXT context = NULL;
	KIRQL             oldIrql;

	OGENINT_KdPrint ( DBGLVL_DEFAULT, ("enter OGENINT_StagedReadWrite()\n"));

	Irp->IoStatus.Information = 0;

	// Can't accept a new io request if:
	//  1) device is removed, 
	//  2) has never been started, 
	//  3) is stopped,
	//  4) has a remove request pending,
	//  5) has a stop device pending
	if ( !OGENINT_CanAcceptIoRequests( DeviceObject ) ) {
		ntStatus = STATUS_DELETE_PENDING;
		Irp->IoStatus.Status = ntStatus;
		OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() can't accept requests, returning STATUS_INSUFFICIENT_RESOURCES\n"));
		Irp->CancelRoutine = NULL;	
		IoCompleteRequest (Irp, IO_NO_INCREMENT );
		return ntStatus;
	}


	irpStack = IoGetCurrentIrpStackLocation (Irp);
	fileObject = irpStack->FileObject;

	pipeHandle =  fileObject->FsContext;

	if (!pipeHandle) {
		ntStatus = STATUS_INVALID_HANDLE;
		OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() Rejecting on invalid pipeHandle 0x%x decimal %d\n",pipeHandle, pipeHandle ));
		Irp->IoStatus.Status = ntStatus;
		Irp->CancelRoutine = NULL;
		IoCompleteRequest (Irp, IO_NO_INCREMENT );
		return ntStatus;
	}

	//
	// submit the request to USB
	//


	if ( Irp->MdlAddress )
		totalLength = MmGetMdlByteCount(Irp->MdlAddress);

		OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() totalLength = decimal %d, Irp->MdlAddress = 0x%x\n",totalLength, Irp->MdlAddress ));

	if ( 0 == totalLength ) {
		// allow 0-len read or write; just return success
		ntStatus = STATUS_SUCCESS;
		Irp->IoStatus.Status = ntStatus;
		OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() returning quick success on zero-len read/write request\n"));
		Irp->CancelRoutine = NULL;
		IoCompleteRequest (Irp, IO_NO_INCREMENT );
		return ntStatus;
	}
    
    if ( UsbdPipeTypeControl == pipeHandle->PipeType) {   
		ChunkSize = 1024;
	}

	// calculate total # of staged irps that will be needed
	totalIrpsNeeded =  totalLength / ChunkSize ;
	if ( totalLength % ChunkSize ) {
		totalIrpsNeeded++;
	}

	if (deviceExtension->PendingIoIrps[Read] || 
		deviceExtension->BaseIrp[Read] ||
		(UsbdPipeTypeControl == pipeHandle->PipeType &&
		 deviceExtension->PendingIoIrps[!Read])) 
	{
		// this should have been cleaned up last time
		// But this could be due to a OVERLAPPED FileRead() called
		// a second time before the first one has been complete !
		// So we reset everything ! :-(
		OGENINT_CancelPendingIo(DeviceObject);
		// try to reset the pipe on error ( unless device has been suddenly removed )
		if ( pipeHandle  && OGENINT_CanAcceptIoRequests( DeviceObject ) ) {

			resetPipeStatus = OGENINT_ResetPipe(DeviceObject, pipeHandle );

			if( !NT_SUCCESS(resetPipeStatus) ) {
				// if can't reset pipe, try to reset device ( parent port )
			   
				resetPipeStatus = OGENINT_ResetDevice(DeviceObject);
			}
			if( !NT_SUCCESS(resetPipeStatus) ) {
     				ntStatus = STATUS_LINK_TIMEOUT;
       				Irp->IoStatus.Status = ntStatus;
					Irp->CancelRoutine = NULL;
       				IoCompleteRequest (Irp, IO_NO_INCREMENT );
       				return ntStatus;
			}
		}    
	}


	used = 0;
	// alloc one extra for termination
	arraySize =  ( totalIrpsNeeded +1 ) * sizeof(OGENINT_RW_CONTEXT);

	// allocate space for an array of OGENINT_RW_CONTEXT structs for the staged irps
	deviceExtension->PendingIoIrps[Read] = OGENINT_ExAllocatePool(NonPagedPool, arraySize );
    
	deviceExtension->arraySize[Read] = 0;
	if ( !deviceExtension->PendingIoIrps[Read] ) {
		ntStatus = STATUS_INSUFFICIENT_RESOURCES;
		OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() !deviceExtension->PendingIoIrps STATUS_INSUFFICIENT_RESOURCES\n"));
		Irp->IoStatus.Status = ntStatus;
		Irp->CancelRoutine = NULL;
		IoCompleteRequest (Irp, IO_NO_INCREMENT );
		return ntStatus;
	}

	deviceExtension->arraySize[Read] = arraySize;
	RtlZeroMemory(deviceExtension->PendingIoIrps[Read], arraySize );

	// init ptr to 1st OGENINT_RW_CONTEXT struct in array
	pCon =  (PUCHAR) deviceExtension->PendingIoIrps[Read];

	deviceExtension->BaseIrp[Read] = Irp; // this is the original user's irp
	deviceExtension->StagedBytesTransferred[Read] = 0;
	deviceExtension->StagedPendingIrpCount[Read] = totalIrpsNeeded;

	// we need to build a series of irps & urbs to represent 
	// this request.

	if (Read) {
    		IoSetCancelRoutine(Irp, OGENINT_CancelRead);
	} else {
		IoSetCancelRoutine(Irp, OGENINT_CancelWrite);
	}
	freeMdls(deviceExtension);
	deviceExtension->NbMdls = 0;
	deviceExtension->Mdls = OGENINT_ExAllocatePool(NonPagedPool,  totalIrpsNeeded * sizeof(PMDL));
	while (NT_SUCCESS(ntStatus) ) {
		context =  (POGENINT_RW_CONTEXT) pCon;
		irp = NULL;
		urb = NULL;
		mdl = NULL;

		if ( !OGENINT_CanAcceptIoRequests( DeviceObject ) || Irp->Cancel ) {
			// got sudden remove! ( i.e. plug was yanked )
			ntStatus = STATUS_DELETE_PENDING;
			Irp->IoStatus.Status = ntStatus;
			OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("OGENINT_StagedReadWrite() got sudden remove, breaking out of URB-building loop\n"));
			break;
		}

		stackSize = (CCHAR)(deviceExtension->TopOfStackDeviceObject->StackSize + 1);
		irp = IoAllocateIrp(stackSize, FALSE);
        
		// Get the virtual address for the buffer described by 
		// our original input Irp's MDL. 
		va = MmGetMdlVirtualAddress(Irp->MdlAddress);

		if (irp) {
			// Each new Irp will 'see' the entire buffer, but map it's IO location
			// to a single ChunkSize section within it via IoBuildPartialMdl()
			mdl = IoAllocateMdl(va,
                                totalLength,
                                FALSE,
                                FALSE,
                                irp);
		}                                    
                            
		if (mdl) {       
			deviceExtension->Mdls[deviceExtension->NbMdls] = mdl;
			deviceExtension->NbMdls++;
			// see if we're done yet
			if( ( used + ChunkSize ) > totalLength  ) {
				// make sure to truncate last transfer if neccy
				ChunkSize = totalLength - used;
			}

			// Map the sub-area of the full user buffer this staged Irp will be using for IO
			IoBuildPartialMdl(Irp->MdlAddress, // Points to an MDL describing the original buffer,
                                               // of which a subrange is to be mapped
                              mdl,             // our allocated target mdl
                              (PUCHAR)va + used, // base virtual address of area to be mapped
                              ChunkSize);      // size of area to be mapped

			used+=ChunkSize;
                	if ( UsbdPipeTypeInterrupt  == pipeHandle->PipeType ||
				UsbdPipeTypeBulk  == pipeHandle->PipeType) 
			{   
				urb = OGENINT_BuildAsyncRequest(DeviceObject,
                                           irp,
                                           pipeHandle,
                                           Read);
			} else if (UsbdPipeTypeControl == pipeHandle->PipeType) {
				urb = OGENINT_BuildAsyncControlRequest(DeviceObject,
                                           irp,
                                           pipeHandle,
                                           Read);
			} else if (UsbdPipeTypeIsochronous  == pipeHandle->PipeType) {
				urb = OGENINT_BuildAsyncIsoRequest(DeviceObject,
                                           irp,
                                           pipeHandle,
                                           Read);
			}
		}
        
		if (urb && irp && mdl) {

			context->Urb = urb;
			context->DeviceObject = DeviceObject;
			context->Irp = irp;
			nIrps++;
        
			// IoGetNextIrpStackLocation gives a higher level driver access to the next-lower 
			// driver's I/O stack location in an IRP so the caller can set it up for the lower driver.
			nextStack = IoGetNextIrpStackLocation(irp);
			if (nextStack == NULL  || DeviceObject->StackSize<=1) {
				ntStatus = STATUS_INSUFFICIENT_RESOURCES;
				OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() !nextStack STATUS_INSUFFICIENT_RESOURCES\n"));
				Irp->IoStatus.Status = ntStatus;
				Irp->CancelRoutine = NULL;
				IoCompleteRequest (Irp, IO_NO_INCREMENT );
				return ntStatus;
			}


			nextStack->MajorFunction = IRP_MJ_INTERNAL_DEVICE_CONTROL;
			nextStack->Parameters.Others.Argument1 = urb;
			nextStack->Parameters.DeviceIoControl.IoControlCode =
				IOCTL_INTERNAL_USB_SUBMIT_URB;
                	if ( UsbdPipeTypeInterrupt  == pipeHandle->PipeType ||
				UsbdPipeTypeBulk  == pipeHandle->PipeType) 
			{   
				IoSetCompletionRoutine(irp,
					Read ? OGENINT_AsyncRead_Complete : OGENINT_AsyncWrite_Complete,
					context, // pass the context array element to completion routine
					TRUE,    // invoke on success
					TRUE,    // invoke on error
					TRUE);   // invoke on cancellation of the Irp	    
	    	
			} else if (UsbdPipeTypeControl == pipeHandle->PipeType) {
				IoSetCompletionRoutine(irp,
					Read ? OGENINT_AsyncControlRead_Complete : OGENINT_AsyncControlWrite_Complete,
					context, // pass the context array element to completion routine
					TRUE,    // invoke on success
					TRUE,    // invoke on error
					TRUE);   // invoke on cancellation of the Irp	    
			
			} else if (UsbdPipeTypeIsochronous  == pipeHandle->PipeType) {
				// FIXME
			}

			OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("OGENINT_StagedReadWrite() created staged irp #%d %x\n", nIrps, irp));
                                   
			// We keep an array of all pending read/write Irps; we may have to cancel
			// them explicitly on sudden device removal or other error
			(( POGENINT_RW_CONTEXT) pCon)->Irp = irp;


			OGENINT_IncrementIoCount(DeviceObject);
			ntStatus = IoCallDriver(
				deviceExtension->TopOfStackDeviceObject,
             			(( POGENINT_RW_CONTEXT) pCon)->Irp);

		} else {
			ntStatus = STATUS_INSUFFICIENT_RESOURCES;
			OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() Dumped from irp loop on failed Irp or urb allocate\n"));
			break;
		}

		if (used >= totalLength) {
			break;      // we're done
		}


		// point to next OGENINT_RW_CONTEXT struct
		pCon +=  sizeof(OGENINT_RW_CONTEXT);

	} // end while



	if (!NT_SUCCESS(ntStatus)) {

		OGENINT_KdPrint ( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() FAILED, ntStatus = 0x%x\n", ntStatus));

		// try to reset the pipe on error ( unless device has been suddenly removed )
		if ( pipeHandle  && OGENINT_CanAcceptIoRequests( DeviceObject ) ) {

			resetPipeStatus = OGENINT_ResetPipe(DeviceObject, pipeHandle );

			OGENINT_KdPrint( DBGLVL_DEFAULT, ("OGENINT_StagedReadWrite() Tried to reset pipe 0x%x, Status = 0x%x\n", pipeHandle, resetPipeStatus));
			OGENINT_KdPrintCond ( DBGLVL_DEFAULT, (!NT_SUCCESS(resetPipeStatus)), ("OGENINT_StagedReadWrite() OGENINT_ResetPipe() FAILED\n"));

			if( !NT_SUCCESS(resetPipeStatus) ) {
				// if can't reset pipe, try to reset device ( parent port )
				OGENINT_KdPrint( DBGLVL_DEFAULT, ("Will try to reset device \n"));

				resetPipeStatus = OGENINT_ResetDevice(DeviceObject);

				OGENINT_KdPrintCond ( DBGLVL_DEFAULT, (!NT_SUCCESS(resetPipeStatus)), ("OGENINT_StagedReadWrite() OGENINT_ResetDevice() FAILED\n"));
			}
		}
	} // end, if !NT_SUCCESS( ntStatus )

	KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[Read], &oldIrql);
	Irp->IoStatus.Status = ntStatus;
	if ( 0 == nIrps ) {
		// only complete the request here if we created no staged irps
		OGENINT_KdPrint ( DBGLVL_HIGH, ("OGENINT_StagedReadWrite() 0 irps staged, completing  base IRP now!\n"));
		Irp->CancelRoutine = NULL;
		IoCompleteRequest (Irp, IO_NO_INCREMENT  );
	} else if (Irp->Cancel) {
		ntStatus = STATUS_CANCELLED;
	} else {
		OGENINT_KdPrint ( DBGLVL_HIGH, ("OGENINT_StagedReadWrite() %d irps staged\n", nIrps));

		if ( deviceExtension->BaseIrp[Read]  &&  Irp == deviceExtension->BaseIrp[Read]) {
			//
			// Mark the original input Irp pending; it will be completed when the last staged irp
			//  is handled ( in OGENINT_AsyncReadWrite_Complete() ).
			//
			OGENINT_KdPrint ( DBGLVL_HIGH, ("OGENINT_StagedReadWrite(),marking base IRP  0x%x pending!\n", Irp));
			ntStatus = STATUS_PENDING;
			Irp->IoStatus.Status = ntStatus;
			if (KeGetCurrentIrql() <= DISPATCH_LEVEL) {
				IoMarkIrpPending(Irp);
			}
		} else {
			// It is possible for OGENINT_AsyncReadWrite_Complete() to have completed the
			//  original irp before we even get here! 
			// If this happens, it will have NULLED-out deviceExtension->BaseIrp.
			ntStatus = STATUS_SUCCESS;
		}
	}
	KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[Read], oldIrql);

	OGENINT_KdPrint ( DBGLVL_HIGH, ("OGENINT_StagedReadWrite() StagedReadWrite  ntStatus = 0x%x decimal %d\n", ntStatus, ntStatus));
	OGENINT_KdPrint ( DBGLVL_HIGH, ("EXIT OGENINT_StagedReadWrite() gExAllocCount = dec %d\n", gExAllocCount ));

	return ntStatus;
}



PURB
OGENINT_BuildAsyncRequest(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PUSBD_PIPE_INFORMATION PipeHandle,
    IN BOOLEAN Read
    )
/*++

Routine Description:

    Called from OGENINT_StageReadWrite() for IRP_MJ_READ or IRP_MJ_WRITE

Arguments:

    DeviceObject - pointer to the FDO ( Functional Device Object )

    Irp - A staged IRP allocated and mapped by this driver in OGENINT_StageReadWrite()
          to perform a single deviceExtension->MaximumTransferSize IO request

    PipeHandle - handle to the endpoint we're reading or writing

    Read - TRUE for reads, FALSE for writes

Return Value:

    ptr to initialized async urb. ( USB Request Block )

--*/
{
	ULONG siz;
	ULONG length;
	PURB urb = NULL;

	(void)DeviceObject;
	length = MmGetMdlByteCount(Irp->MdlAddress);

	siz = sizeof(struct _URB_BULK_OR_INTERRUPT_TRANSFER);
	urb = OGENINT_ExAllocatePool(NonPagedPool, siz);

	OGENINT_KdPrint( DBGLVL_MAXIMUM,("Enter OGENINT_BuildAsyncRequest() len = 0x%x decimal %d \n siz = 0x%x urb 0x%x\n Pipehandle 0x%x\n", length, length, siz, urb, PipeHandle));

	if (urb) {
		RtlZeroMemory(urb, siz);

		urb->UrbBulkOrInterruptTransfer.Hdr.Length = (USHORT) siz;
		urb->UrbBulkOrInterruptTransfer.Hdr.Function =
			URB_FUNCTION_BULK_OR_INTERRUPT_TRANSFER;
		urb->UrbBulkOrInterruptTransfer.PipeHandle =
			PipeHandle->PipeHandle;
		urb->UrbBulkOrInterruptTransfer.TransferFlags =
			Read ? USBD_TRANSFER_DIRECTION_IN : 0;

		// short packet is not treated as an error.
		urb->UrbBulkOrInterruptTransfer.TransferFlags |= 
		USBD_SHORT_TRANSFER_OK;            
                
		//
		// not using linked urb's
		//
		urb->UrbBulkOrInterruptTransfer.UrbLink = NULL;

		urb->UrbBulkOrInterruptTransfer.TransferBufferMDL =
			Irp->MdlAddress;
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength =
			length;

		OGENINT_KdPrint( DBGLVL_MAXIMUM,("OGENINT_BuildAsyncRequest() Init async urb Length = 0x%x decimal %d, buf = 0x%x\n",
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
		urb->UrbBulkOrInterruptTransfer.TransferBuffer));
	}

	OGENINT_KdPrint( DBGLVL_MAXIMUM,("exit OGENINT_BuildAsyncRequest\n"));

	return urb;
}

PURB
OGENINT_BuildAsyncControlRequest(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PUSBD_PIPE_INFORMATION PipeHandle,
    IN BOOLEAN Read
    )
/*++

Routine Description:

    Called from OGENINT_StageReadWrite() for IRP_MJ_READ or IRP_MJ_WRITE

Arguments:

    DeviceObject - pointer to the FDO ( Functional Device Object )

    Irp - A staged IRP allocated and mapped by this driver in OGENINT_StageReadWrite()
          to perform a single deviceExtension->MaximumTransferSize IO request

    PipeHandle - handle to the endpoint we're reading or writing

    Read - TRUE for reads, FALSE for writes

Return Value:

    ptr to initialized async urb. ( USB Request Block )

--*/
{
	ULONG siz;
	ULONG length;
	PURB urb = NULL;

	(void)DeviceObject;
	length = MmGetMdlByteCount(Irp->MdlAddress);

	siz = sizeof(struct _URB_CONTROL_TRANSFER);
	urb = OGENINT_ExAllocatePool(NonPagedPool, siz);

	OGENINT_KdPrint( DBGLVL_MAXIMUM,("Enter OGENINT_BuildAsyncControlRequest() len = 0x%x decimal %d \n siz = 0x%x urb 0x%x\n Pipehandle 0x%x\n", length, length, siz, urb, PipeHandle));

	if (urb) {
		RtlZeroMemory(urb, siz);

		urb->UrbControlTransfer.Hdr.Length = (USHORT) siz;
		urb->UrbControlTransfer.Hdr.Function =
			URB_FUNCTION_CONTROL_TRANSFER;
		urb->UrbControlTransfer.PipeHandle =
			PipeHandle->PipeHandle;
		urb->UrbControlTransfer.TransferFlags =
			Read ? USBD_TRANSFER_DIRECTION_IN : 0;
		urb->UrbControlTransfer.SetupPacket[0] = ((Read << 7) & 0x80) | 0x42;
		urb->UrbControlTransfer.SetupPacket[1] = 0;
		urb->UrbControlTransfer.SetupPacket[2] = 0;
		urb->UrbControlTransfer.SetupPacket[3] = 0;
		urb->UrbControlTransfer.SetupPacket[4] = 0;
		urb->UrbControlTransfer.SetupPacket[5] = 0;
		urb->UrbControlTransfer.SetupPacket[6] = (UCHAR)length & 0xFF;
		urb->UrbControlTransfer.SetupPacket[7] = (UCHAR) (length >> 8)  & 0xFF;

		// short packet is not treated as an error.
		urb->UrbControlTransfer.TransferFlags |= 
		USBD_SHORT_TRANSFER_OK;            
                
		//
		// not using linked urb's
		//
		urb->UrbControlTransfer.UrbLink = NULL;

		urb->UrbControlTransfer.TransferBufferMDL =
			Irp->MdlAddress;
		urb->UrbControlTransfer.TransferBufferLength =
			length;

		OGENINT_KdPrint( DBGLVL_MAXIMUM,("OGENINT_BuildAsyncControlRequest() Init async urb Length = 0x%x decimal %d, buf = 0x%x\n",
		urb->UrbControlTransfer.TransferBufferLength,
		urb->UrbControlTransfer.TransferBufferLength,
		urb->UrbControlTransfer.TransferBuffer));
	}

	OGENINT_KdPrint( DBGLVL_MAXIMUM,("exit OGENINT_BuildAsyncControlRequest\n"));

	return urb;
}

// FIXME
PURB
OGENINT_BuildAsyncIsoRequest(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PUSBD_PIPE_INFORMATION PipeHandle,
    IN BOOLEAN Read
    )
/*++

Routine Description:

    Called from OGENINT_StageReadWrite() for IRP_MJ_READ or IRP_MJ_WRITE

Arguments:

    DeviceObject - pointer to the FDO ( Functional Device Object )

    Irp - A staged IRP allocated and mapped by this driver in OGENINT_StageReadWrite()
          to perform a single deviceExtension->MaximumTransferSize IO request

    PipeHandle - handle to the endpoint we're reading or writing

    Read - TRUE for reads, FALSE for writes

Return Value:

    ptr to initialized async urb. ( USB Request Block )

--*/
{
	ULONG siz;
	ULONG length;
	PURB urb = NULL;

	(void)DeviceObject;
	length = MmGetMdlByteCount(Irp->MdlAddress);

	siz = sizeof(struct _URB_BULK_OR_INTERRUPT_TRANSFER);
	urb = OGENINT_ExAllocatePool(NonPagedPool, siz);

	OGENINT_KdPrint( DBGLVL_MAXIMUM,("Enter OGENINT_BuildAsyncRequest() len = 0x%x decimal %d \n siz = 0x%x urb 0x%x\n Pipehandle 0x%x\n", length, length, siz, urb, PipeHandle));

	if (urb) {
		RtlZeroMemory(urb, siz);

		urb->UrbBulkOrInterruptTransfer.Hdr.Length = (USHORT) siz;
		urb->UrbBulkOrInterruptTransfer.Hdr.Function =
			URB_FUNCTION_BULK_OR_INTERRUPT_TRANSFER;
		urb->UrbBulkOrInterruptTransfer.PipeHandle =
			PipeHandle->PipeHandle;
		urb->UrbBulkOrInterruptTransfer.TransferFlags =
			Read ? USBD_TRANSFER_DIRECTION_IN : 0;

		// short packet is not treated as an error.
		urb->UrbBulkOrInterruptTransfer.TransferFlags |= 
		USBD_SHORT_TRANSFER_OK;            
                
		//
		// not using linked urb's
		//
		urb->UrbBulkOrInterruptTransfer.UrbLink = NULL;

		urb->UrbBulkOrInterruptTransfer.TransferBufferMDL =
			Irp->MdlAddress;
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength =
			length;

		OGENINT_KdPrint( DBGLVL_MAXIMUM,("OGENINT_BuildAsyncRequest() Init async urb Length = 0x%x decimal %d, buf = 0x%x\n",
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
		urb->UrbBulkOrInterruptTransfer.TransferBuffer));
	}

	OGENINT_KdPrint( DBGLVL_MAXIMUM,("exit OGENINT_BuildAsyncRequest\n"));

	return urb;
}

NTSTATUS
OGENINT_AsyncWrite_Complete(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PVOID Context
    )
/*++

Routine Description:

  Completion routine for our staged read/write Irps


Arguments:

    DeviceObject - Pointer to the device object for next lower device
	in the  driver stack; 

    Irp - Irp completed.

    Context - Driver defined context.

Return Value:

    The function value is the final status from the operation.

--*/
{
	NTSTATUS			ntStatus = STATUS_SUCCESS;
	PURB				urb;
	POGENINT_RW_CONTEXT context = Context;
//	PIO_STACK_LOCATION	irpStack;
	PDEVICE_OBJECT      deviceObject;
	PDEVICE_EXTENSION   deviceExtension;
	KIRQL             oldIrql;
	PIRP op;

	(void)DeviceObject;	
	// We have to get the deviceObject from the context, since the DeviceObject passed in
	//  here belongs to the next lower driver in the stack because we were invoked via
	//   IoCallDriver in OGENINT_StagedReadWrite()
	deviceObject = context->DeviceObject;
	deviceExtension = deviceObject->DeviceExtension;

	//  If the lower driver returned PENDING, mark our stack location as pending also.
	if ( Irp->PendingReturned ) {  
		if (KeGetCurrentIrql() <= DISPATCH_LEVEL) {
			IoMarkIrpPending(Irp);
		}
	}
	
	if (!deviceExtension->PendingIoIrps[0]) return ntStatus;  
	if (!deviceExtension->BaseIrp[0] )  return ntStatus;  

	if ( context->Irp != Irp )  return ntStatus; 
    
	urb = context->Urb;

    
	OGENINT_KdPrint( DBGLVL_HIGH,  ("\n\n ENTER OGENINT_AsyncWrite_Complete():  Length 0x%08X decimal %d\n   Status 0x%08X\n",
                     urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
                     urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
                     urb->UrbHeader.Status));

	// decrement count of staged pending irps
	deviceExtension->StagedPendingIrpCount[0]--;

	// decrement the driver's overall pending irp count
	OGENINT_DecrementIoCount(deviceObject);
    
	// 
	// IoCallDriver has been called on this Irp;
	// Set the length based on the TransferBufferLength
	// value in the URB
	//
	Irp->IoStatus.Information =
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength;

	ntStatus = STATUS_MORE_PROCESSING_REQUIRED;

	deviceExtension->StagedBytesTransferred[0] += 
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength; 
    
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("OGENINT_AsyncWrite_Complete(): Staged Async Completion %d, bytes = %d\n", 
        deviceExtension->StagedPendingIrpCount[0],
        deviceExtension->StagedBytesTransferred[0])); 

	op = context->Irp;
	KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[0], &oldIrql);
	context->Irp = NULL; 
	KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[0], oldIrql);
	IoFreeIrp(op);


	if (deviceExtension->StagedPendingIrpCount[0] == 0) {
    		KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[0], &oldIrql);
		OGENINT_KdPrint ( DBGLVL_HIGH,("OGENINT_AsyncWrite_Complete(): StagedPendingIrpCount == 0, completeting BaseIrp 0x%x\n    Total bytes xferred = 0x%x, decimal %d\n", deviceExtension->BaseIrp[0], deviceExtension->StagedBytesTransferred[0], deviceExtension->StagedBytesTransferred[0]));

		deviceExtension->BaseIrp[0]->IoStatus.Status = STATUS_SUCCESS; 

		deviceExtension->BaseIrp[0]->IoStatus.Information = 
		deviceExtension->StagedBytesTransferred[0];
		deviceExtension->BaseIrp[0]->CancelRoutine = NULL;

		IoCompleteRequest(deviceExtension->BaseIrp[0],
                          IO_NO_INCREMENT);

    		deviceExtension->arraySize[0] = 0;
		OGENINT_ExFreePool( deviceExtension->PendingIoIrps[0] ); 
		deviceExtension->PendingIoIrps[0]  = NULL;
		deviceExtension->BaseIrp[0] = NULL;

		// the event is only waited on if OGENINT_CancelPendingIo() has been called
		KeSetEvent(&deviceExtension->StagingDoneEvent, 1, FALSE);
    		KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[0], oldIrql);
	}
	OGENINT_ExFreePool(urb);

	OGENINT_KdPrint ( DBGLVL_HIGH, ("Exit OGENINT_AsyncWrite_Complete() gExAllocCount = dec %d\n", gExAllocCount ));
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("Exit OGENINT_AsyncWrite_Complete(), ntStatus = 0x%x\n\n",ntStatus )); 
	return ntStatus;
}


NTSTATUS
OGENINT_AsyncRead_Complete(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PVOID Context
    )
/*++

Routine Description:

  Completion routine for our staged read/write Irps


Arguments:

    DeviceObject - Pointer to the device object for next lower device
	in the  driver stack; 

    Irp - Irp completed.

    Context - Driver defined context.

Return Value:

    The function value is the final status from the operation.

--*/
{
	NTSTATUS			ntStatus = STATUS_SUCCESS;
	PURB				urb;
	POGENINT_RW_CONTEXT context = Context;
	//PIO_STACK_LOCATION	irpStack;
	PDEVICE_OBJECT      deviceObject;
	PDEVICE_EXTENSION   deviceExtension;
	KIRQL             oldIrql;
	PIRP op;

	(void)DeviceObject;	
	// We have to get the deviceObject from the context, since the DeviceObject passed in
	//  here belongs to the next lower driver in the stack because we were invoked via
	//   IoCallDriver in OGENINT_StagedReadWrite()
	deviceObject = context->DeviceObject;
	deviceExtension = deviceObject->DeviceExtension;

	//  If the lower driver returned PENDING, mark our stack location as pending also.
	if ( Irp->PendingReturned ) {  
		if (KeGetCurrentIrql() <= DISPATCH_LEVEL) {
			IoMarkIrpPending(Irp);
		}
	}
	
	if (!deviceExtension->PendingIoIrps[1]) return ntStatus;  
	if (!deviceExtension->BaseIrp[1] )  return ntStatus;  

	if ( context->Irp != Irp )  return ntStatus; 
    
	urb = context->Urb;

    
	OGENINT_KdPrint( DBGLVL_HIGH,  ("\n\n ENTER OGENINT_AsyncRead_Complete():  Length 0x%08X decimal %d\n   Status 0x%08X\n",
                     urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
                     urb->UrbBulkOrInterruptTransfer.TransferBufferLength,
                     urb->UrbHeader.Status));

	// decrement count of staged pending irps
	deviceExtension->StagedPendingIrpCount[1]--;

	// decrement the driver's overall pending irp count
	OGENINT_DecrementIoCount(deviceObject);
    
	// 
	// IoCallDriver has been called on this Irp;
	// Set the length based on the TransferBufferLength
	// value in the URB
	//
	Irp->IoStatus.Information =
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength;

	ntStatus = STATUS_MORE_PROCESSING_REQUIRED;

	deviceExtension->StagedBytesTransferred[1] += 
		urb->UrbBulkOrInterruptTransfer.TransferBufferLength; 
    
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("OGENINT_AsyncRead_Complete(): Staged Async Completion %d, bytes = %d\n", 
		deviceExtension->StagedPendingIrpCount[1],
		deviceExtension->StagedBytesTransferred[1])); 

	op = context->Irp;
	KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[1], &oldIrql);
	context->Irp = NULL; 
	KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[1], oldIrql);
	IoFreeIrp(op);


	if (deviceExtension->StagedPendingIrpCount[1] == 0) {
    		KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[1], &oldIrql);
		OGENINT_KdPrint ( DBGLVL_HIGH,("OGENINT_AsyncRead_Complete(): StagedPendingIrpCount == 0, completeting BaseIrp 0x%x\n    Total bytes xferred = 0x%x, decimal %d\n", deviceExtension->BaseIrp[1], deviceExtension->StagedBytesTransferred[1], deviceExtension->StagedBytesTransferred[1]));

		deviceExtension->BaseIrp[1]->IoStatus.Status = STATUS_SUCCESS; 

		deviceExtension->BaseIrp[1]->IoStatus.Information = 
		deviceExtension->StagedBytesTransferred[1];
		deviceExtension->BaseIrp[1]->CancelRoutine = NULL;

		IoCompleteRequest(deviceExtension->BaseIrp[1],
                          IO_NO_INCREMENT);

    		deviceExtension->arraySize[1] = 0;
		OGENINT_ExFreePool( deviceExtension->PendingIoIrps[1] ); 
		deviceExtension->PendingIoIrps[1]  = NULL;
		deviceExtension->BaseIrp[1] = NULL;

		// the event is only waited on if OGENINT_CancelPendingIo() has been called
		KeSetEvent(&deviceExtension->StagingDoneEvent, 1, FALSE);
    		KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[1], oldIrql);
	}
	OGENINT_ExFreePool(urb);

	OGENINT_KdPrint ( DBGLVL_HIGH, ("Exit OGENINT_AsyncRead_Complete() gExAllocCount = dec %d\n", gExAllocCount ));
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("Exit OGENINT_AsyncRead_Complete(), ntStatus = 0x%x\n\n",ntStatus )); 
	return ntStatus;
}

NTSTATUS
OGENINT_AsyncControlWrite_Complete(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PVOID Context
    )
/*++

Routine Description:

  Completion routine for our staged read/write Irps


Arguments:

    DeviceObject - Pointer to the device object for next lower device
	in the  driver stack; 

    Irp - Irp completed.

    Context - Driver defined context.

Return Value:

    The function value is the final status from the operation.

--*/
{
	NTSTATUS			ntStatus = STATUS_SUCCESS;
	PURB				urb;
	POGENINT_RW_CONTEXT context = Context;
//	PIO_STACK_LOCATION	irpStack;
	PDEVICE_OBJECT      deviceObject;
	PDEVICE_EXTENSION   deviceExtension;
	KIRQL             oldIrql;
	PIRP op;
	
	// We have to get the deviceObject from the context, since the DeviceObject passed in
	//  here belongs to the next lower driver in the stack because we were invoked via
	//   IoCallDriver in OGENINT_StagedReadWrite()

	(void)DeviceObject;

	deviceObject = context->DeviceObject;
	deviceExtension = deviceObject->DeviceExtension;

	if ( !Irp || context->Irp != Irp )  return ntStatus; 
	if (!deviceExtension->PendingIoIrps[0]) return ntStatus;  
	if (!deviceExtension->BaseIrp[0] )  return ntStatus;  

	//  If the lower driver returned PENDING, mark our stack location as pending also.
/*
	if ( Irp->PendingReturned ) {  
		if (KeGetCurrentIrql() <= DISPATCH_LEVEL &&
			!(Irp->Tail.Overlay.CurrentStackLocation->Control & SL_PENDING_RETURNED)) {
			//Irp->Tail.Overlay.CurrentStackLocation->Control |= SL_PENDING_RETURNED; // !read only !!!
			IoMarkIrpPending(Irp);
		}
	}
  */  
	urb = context->Urb;
    
	OGENINT_KdPrint( DBGLVL_HIGH,  ("\n\n ENTER OGENINT_AsyncWrite_Complete():  Length 0x%08X decimal %d\n   Status 0x%08X\n",
                     urb->UrbControlTransfer.TransferBufferLength,
                     urb->UrbControlTransfer.TransferBufferLength,
                     urb->UrbHeader.Status));

	// decrement count of staged pending irps
	deviceExtension->StagedPendingIrpCount[0]--;

	// decrement the driver's overall pending irp count
	OGENINT_DecrementIoCount(deviceObject);
    
	// 
	// IoCallDriver has been called on this Irp;
	// Set the length based on the TransferBufferLength
	// value in the URB
	//
	Irp->IoStatus.Information =
		urb->UrbControlTransfer.TransferBufferLength;

	ntStatus = STATUS_MORE_PROCESSING_REQUIRED;

	deviceExtension->StagedBytesTransferred[0] += 
		urb->UrbControlTransfer.TransferBufferLength; 
    
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("OGENINT_AsyncWrite_Complete(): Staged Async Completion %d, bytes = %d\n", 
        deviceExtension->StagedPendingIrpCount[0],
        deviceExtension->StagedBytesTransferred[0])); 

	op = context->Irp;
	KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[0], &oldIrql);
	context->Irp = NULL; 
	KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[0], oldIrql);
	IoFreeIrp(op);


	if (deviceExtension->StagedPendingIrpCount[0] == 0) {
    		KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[0], &oldIrql);
		OGENINT_KdPrint ( DBGLVL_HIGH,("OGENINT_AsyncWrite_Complete(): StagedPendingIrpCount == 0, completeting BaseIrp 0x%x\n    Total bytes xferred = 0x%x, decimal %d\n", deviceExtension->BaseIrp[0], deviceExtension->StagedBytesTransferred[0], deviceExtension->StagedBytesTransferred[0]));

		freeMdls(deviceExtension);
		deviceExtension->BaseIrp[0]->IoStatus.Status = STATUS_SUCCESS; 

		deviceExtension->BaseIrp[0]->IoStatus.Information = 
		deviceExtension->StagedBytesTransferred[0];
		deviceExtension->BaseIrp[0]->CancelRoutine = NULL;

		IoCompleteRequest(deviceExtension->BaseIrp[0],
                          IO_NO_INCREMENT);

    		deviceExtension->arraySize[0] = 0;
		OGENINT_ExFreePool( deviceExtension->PendingIoIrps[0] ); 
		deviceExtension->PendingIoIrps[0]  = NULL;
		deviceExtension->BaseIrp[0] = NULL;

		// the event is only waited on if OGENINT_CancelPendingIo() has been called
		KeSetEvent(&deviceExtension->StagingDoneEvent, 1, FALSE);
    		KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[0], oldIrql);
	}
	OGENINT_ExFreePool(urb);

	OGENINT_KdPrint ( DBGLVL_HIGH, ("Exit OGENINT_AsyncWrite_Complete() gExAllocCount = dec %d\n", gExAllocCount ));
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("Exit OGENINT_AsyncWrite_Complete(), ntStatus = 0x%x\n\n",ntStatus )); 
	return ntStatus;
}


NTSTATUS
OGENINT_AsyncControlRead_Complete(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PVOID Context
    )
/*++

Routine Description:

  Completion routine for our staged read/write Irps


Arguments:

    DeviceObject - Pointer to the device object for next lower device
	in the  driver stack; 

    Irp - Irp completed.

    Context - Driver defined context.

Return Value:

    The function value is the final status from the operation.

--*/
{
	NTSTATUS			ntStatus = STATUS_SUCCESS;
	PURB				urb;
	POGENINT_RW_CONTEXT context = Context;
//	PIO_STACK_LOCATION	irpStack;
	PDEVICE_OBJECT      deviceObject;
	PDEVICE_EXTENSION   deviceExtension;
	KIRQL             oldIrql;
	PIRP op;

	(void)DeviceObject;	
	// We have to get the deviceObject from the context, since the DeviceObject passed in
	//  here belongs to the next lower driver in the stack because we were invoked via
	//   IoCallDriver in OGENINT_StagedReadWrite()
	deviceObject = context->DeviceObject;
	deviceExtension = deviceObject->DeviceExtension;

	//  If the lower driver returned PENDING, mark our stack location as pending also.
	if ( Irp->PendingReturned ) {  
		if (KeGetCurrentIrql() <= DISPATCH_LEVEL) {
			IoMarkIrpPending(Irp);
		}
	}
	
	if (!deviceExtension->PendingIoIrps[1]) return ntStatus;  
	if (!deviceExtension->BaseIrp[1] )  return ntStatus;  

	if ( context->Irp != Irp )  return ntStatus; 
    
	urb = context->Urb;

    
	OGENINT_KdPrint( DBGLVL_HIGH,  ("\n\n ENTER OGENINT_AsyncRead_Complete():  Length 0x%08X decimal %d\n   Status 0x%08X\n",
                     urb->UrbControlTransfer.TransferBufferLength,
                     urb->UrbControlTransfer.TransferBufferLength,
                     urb->UrbHeader.Status));

	// decrement count of staged pending irps
	deviceExtension->StagedPendingIrpCount[1]--;

	// decrement the driver's overall pending irp count
	OGENINT_DecrementIoCount(deviceObject);
    
	// 
	// IoCallDriver has been called on this Irp;
	// Set the length based on the TransferBufferLength
	// value in the URB
	//
	Irp->IoStatus.Information =
		urb->UrbControlTransfer.TransferBufferLength;

	ntStatus = STATUS_MORE_PROCESSING_REQUIRED;

	deviceExtension->StagedBytesTransferred[1] += 
		urb->UrbControlTransfer.TransferBufferLength; 
    
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("OGENINT_AsyncRead_Complete(): Staged Async Completion %d, bytes = %d\n", 
		deviceExtension->StagedPendingIrpCount[1],
		deviceExtension->StagedBytesTransferred[1])); 

	op = context->Irp;
	KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[1], &oldIrql);
	context->Irp = NULL; 
	KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[1], oldIrql);
	IoFreeIrp(op);


	if (deviceExtension->StagedPendingIrpCount[1] == 0) {
    		KeAcquireSpinLock (&deviceExtension->IoStatusSpinLock[1], &oldIrql);
		OGENINT_KdPrint ( DBGLVL_HIGH,("OGENINT_AsyncRead_Complete(): StagedPendingIrpCount == 0, completeting BaseIrp 0x%x\n    Total bytes xferred = 0x%x, decimal %d\n", deviceExtension->BaseIrp[1], deviceExtension->StagedBytesTransferred[1], deviceExtension->StagedBytesTransferred[1]));

		deviceExtension->BaseIrp[1]->IoStatus.Status = STATUS_SUCCESS; 

		deviceExtension->BaseIrp[1]->IoStatus.Information = 
		deviceExtension->StagedBytesTransferred[1];
		deviceExtension->BaseIrp[1]->CancelRoutine = NULL;
		freeMdls(deviceExtension);
		
		IoCompleteRequest(deviceExtension->BaseIrp[1],
                          IO_NO_INCREMENT);

    		deviceExtension->arraySize[1] = 0;
		OGENINT_ExFreePool( deviceExtension->PendingIoIrps[1] ); 
		deviceExtension->PendingIoIrps[1]  = NULL;
		deviceExtension->BaseIrp[1] = NULL;

		// the event is only waited on if OGENINT_CancelPendingIo() has been called
		KeSetEvent(&deviceExtension->StagingDoneEvent, 1, FALSE);
    		KeReleaseSpinLock (&deviceExtension->IoStatusSpinLock[1], oldIrql);
	}
	OGENINT_ExFreePool(urb);

	OGENINT_KdPrint ( DBGLVL_HIGH, ("Exit OGENINT_AsyncRead_Complete() gExAllocCount = dec %d\n", gExAllocCount ));
	OGENINT_KdPrint ( DBGLVL_MAXIMUM,("Exit OGENINT_AsyncRead_Complete(), ntStatus = 0x%x\n\n",ntStatus )); 
	return ntStatus;
}


NTSTATUS
OGENINT_Read(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

	This is the IRP_MJ_READ routine set in our dispatch table;
	ReadFile() calls from user mode ultimately land here

Arguments:

    DeviceObject - pointer to the device object for this instance of the 82930
                    device.

    IRP - pointer to the IRP_MJ_READ

Return Value:

    NT status code

--*/
{

	NTSTATUS ntStatus = OGENINT_StagedReadWrite(DeviceObject,
                                  Irp,
                                  TRUE);	// false to write, true to read

	return ntStatus;                                  

}

NTSTATUS
OGENINT_Write(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:
	This is the IRP_MJ_WRITE routine set in our dispatch table;
	WriteFile() calls from user mode ultimately land here

Arguments:

    DeviceObject - pointer to the device object for this instance of the 82930
                    device.

    IRP - pointer to the IRP_MJ_WRITE

Return Value:

    NT status code

--*/
{

	NTSTATUS ntStatus = OGENINT_StagedReadWrite(DeviceObject,
                                  Irp,
                                  FALSE);	// false to write, true to read

	return ntStatus;                                  

}



NTSTATUS
OGENINT_Close(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

    This is the dispatch table routine for IRP_MJ_CLOSE.
    It handles user mode CloseHandle() calls for a pipe
    It closes the File Object for the pipe handle it represents.

Arguments:

    DeviceObject - pointer to our FDO (Functional Device Object )


Return Value:

    NT status code

--*/
{
	NTSTATUS ntStatus;
	NTSTATUS actStat;
	PFILE_OBJECT fileObject;
	PIO_STACK_LOCATION irpStack;
	PDEVICE_EXTENSION deviceExtension;
	PUSBD_PIPE_INFORMATION pipeHandle = NULL;

	OGENINT_KdPrint( DBGLVL_DEFAULT,("entering OGENINT_Close\n"));

    
	OGENINT_IncrementIoCount(DeviceObject);

	deviceExtension = DeviceObject->DeviceExtension;
	irpStack = IoGetCurrentIrpStackLocation (Irp);
	fileObject = irpStack->FileObject;

	if (fileObject->FsContext) {
		// closing pipe handle
		pipeHandle =  fileObject->FsContext;

		if ( pipeHandle->PipeFlags ) { // set if opneed
			// may have been aborted
			OGENINT_KdPrint( DBGLVL_DEFAULT,("closing pipe %x\n", pipeHandle));
			deviceExtension->OpenPipeCount--;
			pipeHandle->PipeFlags = 0;
		} else {
			// pipe was already closed; this can only be if we got a sudden REMOVE_DEVICE
			if (!deviceExtension->DeviceRemoved) {
				OGENINT_KdPrint( DBGLVL_DEFAULT,
				("Houston, we've got a problem...\n"));
			}
			OGENINT_KdPrint( DBGLVL_DEFAULT,("Pipe %x was already closed \n", pipeHandle));
		}
	}

	OGENINT_DecrementIoCount(DeviceObject);
	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;

	ntStatus = Irp->IoStatus.Status;
	Irp->CancelRoutine = NULL;
	IoCompleteRequest (Irp,
                       IO_NO_INCREMENT
                       );
                       
	// try to power down device if this is the last pipe
	actStat = OGENINT_SelfSuspendOrActivate( DeviceObject, TRUE );

	OGENINT_KdPrint( DBGLVL_DEFAULT,("exit OGENINT_Close OpenPipeCount = decimal %d, status %x\n",deviceExtension->OpenPipeCount, ntStatus));

	return ntStatus;
}


NTSTATUS
OGENINT_Create(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

    This is the dispatch table routine for IRP_MJ_CREATE.
    It's the entry point for CreateFile() calls
    user mode apps may open "<name genned fron GUID>.\yy"
    where yy is the internal pipe id

Arguments:

    DeviceObject - pointer to our FDO ( Functional Device Object )


Return Value:

    NT status code

--*/
{
	NTSTATUS ntStatus = STATUS_SUCCESS;
	PFILE_OBJECT fileObject;
	PIO_STACK_LOCATION irpStack;
	PDEVICE_EXTENSION deviceExtension;
	ULONG i, nameLen, ix, uval, umultiplier;
	NTSTATUS actStat;
	PUSBD_INTERFACE_INFORMATION interface;
	PUSBD_PIPE_INFORMATION PipeInfo;

	deviceExtension = DeviceObject->DeviceExtension;
	interface = deviceExtension->UsbInterface;

	OGENINT_KdPrint( DBGLVL_DEFAULT,("entering OGENINT_Create\n"));

	OGENINT_IncrementIoCount(DeviceObject);

	// Can't accept a new io request if:
	//  1) device is removed, 
	//  2) has never been started, 
	//  3) is stopped,
	//  4) has a remove request pending,
	//  5) has a stop device pending
	if ( !OGENINT_CanAcceptIoRequests( DeviceObject ) ) {
		ntStatus = STATUS_DELETE_PENDING;
		Irp->IoStatus.Status = ntStatus;
		Irp->IoStatus.Information = 0;

		Irp->CancelRoutine = NULL;
		IoCompleteRequest (Irp,
                           IO_NO_INCREMENT
                          );

		OGENINT_DecrementIoCount(DeviceObject);                          
        
		OGENINT_KdPrint( DBGLVL_DEFAULT,("ABORTING OGENINT_Create\n"));
		return ntStatus;
	}
    
	irpStack = IoGetCurrentIrpStackLocation (Irp);
	fileObject = irpStack->FileObject;

	// fscontext is null for device
	fileObject->FsContext = NULL;

	nameLen = 0;
	while (fileObject->FileName.Buffer && 
		fileObject->FileName.Buffer[nameLen])
	{
		nameLen++;
	}
		
	//nameLen = fileObject->FileName.Length; returns the byte length!!!

	if (nameLen != 0) {

		OGENINT_KdPrint( DBGLVL_DEFAULT,("OGENINT_Create fileObject->FileName = %ws\n", fileObject->FileName.Buffer ));

		// Get pipe# to open
		ix = nameLen -1;  // index last char of pipe name

		// if last char isn't digit, decrement till it is
		while( ( (fileObject->FileName.Buffer[ ix ] < (WCHAR) '0') ||
			(fileObject->FileName.Buffer[ ix ] > (WCHAR) '9') ) 
			&& ix )
		{
			ix--;  
		}
		OGENINT_ASSERT( ix ); 
		if (  !ix  )  {  //  filename better have had at least one ascii digit!    
			ntStatus = STATUS_INVALID_PARAMETER;
		} else {

			//
			// A name was specified, convert it to a pipe id.
			// Parse the ansi ascii decimal 0-based pipe number 
			//
			uval = 0;
			umultiplier = 1;
			// we're traversing least-to-most significant digits
			while( ( (fileObject->FileName.Buffer[ ix ] >= (WCHAR) '0') &&
				(fileObject->FileName.Buffer[ ix ] <= (WCHAR) '9') ) 
				&& ix ) 
			{

				uval +=  (umultiplier *
					     (ULONG) (fileObject->FileName.Buffer[ ix ] - (WCHAR) '0'));
				OGENINT_KdPrint( DBGLVL_DEFAULT,("pipe %d (%d) fl %ws\n", ix, uval, fileObject->FileName.Buffer + ix));
				ix--;
				umultiplier *= 10; 
			}

			// init status to bad; will set good in below loop on success
			ntStatus = STATUS_INSUFFICIENT_RESOURCES;

			for (i=0; i<interface->NumberOfPipes; i++) {

				PipeInfo =  &interface->Pipes[i]; // PUSBD_PIPE_INFORMATION  PipeInfo;
				OGENINT_KdPrint( DBGLVL_DEFAULT,("pipe %d (%d) flag %x\n", i, uval, PipeInfo->PipeFlags));

				// find the corresponding unopened pipe
				if ( ( uval == i) && !PipeInfo->PipeFlags) { // PipeFlags set if open
					//
					// found a match
					//
					OGENINT_KdPrint( DBGLVL_DEFAULT,("open pipe %d\n", uval));
					fileObject->FsContext = PipeInfo;
					PipeInfo->PipeFlags = TRUE; // set flag for opened
					ntStatus = STATUS_SUCCESS;

					deviceExtension->OpenPipeCount++;

					// try to power up device if its not in D0
					actStat = OGENINT_SelfSuspendOrActivate( DeviceObject, FALSE );
					break;
				}
			}
		}

	} // if ix

	Irp->IoStatus.Status = ntStatus;
	Irp->IoStatus.Information = 0;
	Irp->CancelRoutine = NULL;
	IoCompleteRequest (Irp,
                       IO_NO_INCREMENT
                       );

	OGENINT_DecrementIoCount(DeviceObject);                               

	OGENINT_KdPrint( DBGLVL_DEFAULT,("exit OGENINT_Create %x\n", ntStatus));

	return ntStatus;
}




BOOLEAN
OGENINT_CancelPendingIo(
    IN PDEVICE_OBJECT DeviceObject
    )
/*++

Routine Description:
	Cancels pending IO, as on a sudden IRP_MN_REMOVE_DEVICE 
	This driver maintains and array of info structs (OGENINT_RW_CONTEXT)
	on self-generated IRPS for staged read/writes; This routine traverses
	it and cancels all pending IO irps

Arguments:

    DeviceObject - pointer to the device object for this instance of the 82930
                    device.


Return Value:

    TRUE if cancelled any, else FALSE

--*/
{
	PDEVICE_EXTENSION deviceExtension = DeviceObject->DeviceExtension;
	PUCHAR pCon =  (PUCHAR) deviceExtension->PendingIoIrps[0];
	ULONG i = 0;
	PIRP Irp;
	USHORT uDriverCancel = 0;  // count cancelled via iocancelirp()
	BOOLEAN cRes = 0; 
	//NTSTATUS ntStatus, 
	NTSTATUS waitStatus;

	// nothing pending
	if (deviceExtension->PendingIoIrps[0] ) {

		OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("enter OGENINT_CancelPendingIo()\n"));

		// the OGENINT_RW_CONTEXT array is terminated by an entry with a NULL Irp
		for ( i = 0; i < deviceExtension->arraySize[0] / sizeof(OGENINT_RW_CONTEXT) ;  i++ ) {

			Irp = (((POGENINT_RW_CONTEXT) pCon) + i)->Irp;
			if (!Irp) continue;
			//
			// Since IoCallDriver has been called on this request, we call IoCancelIrp
			//  and let our completion routine handle it
			//
			cRes = IoCancelIrp( Irp );

			OGENINT_KdPrint ( DBGLVL_MAXIMUM,  ("OGENINT_CancelPendingIo() IoCancelIrp() cRes=%d, IRP 0x%x\n", cRes, Irp));

			// if cancel call failed, they all will, so dump out
			if ( !cRes ) break;

			uDriverCancel++; // flag we tried to cancel at least one

			// point to next context struct in array
			pCon +=  sizeof( OGENINT_RW_CONTEXT);
		} // end, for

		if ( uDriverCancel && cRes ) {

			// We only get here if we cancelled at least one and all cancellations were successfull.
			// Wait on the event set on last cancel in OGENINT_AsyncReadWriteComplete();
			OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("OGENINT_CancelPendingIo() before waiting for StagingDoneEvent()\n" ));
			waitStatus = KeWaitForSingleObject(
				&deviceExtension->StagingDoneEvent,
				Suspended,
				KernelMode,
				FALSE,
				NULL);
			OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("OGENINT_CancelPendingIo() finished waiting for StagingDoneEvent()\n" ));
		}

		OGENINT_KdPrintCond ( DBGLVL_HIGH, uDriverCancel,
			("OGENINT_CancelPendingIo() cancelled %d via IoCancelIrp()\n",uDriverCancel));
	} else if (deviceExtension->PendingIoIrps[1]) {
		pCon =  (PUCHAR) deviceExtension->PendingIoIrps[1];
		i = 0;
		uDriverCancel = 0;  // count cancelled via iocancelirp()

		OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("enter OGENINT_CancelPendingIo()\n"));

		// the OGENINT_RW_CONTEXT array is terminated by an entry with a NULL Irp
		for ( i = 0; i < deviceExtension->arraySize[1] / sizeof(OGENINT_RW_CONTEXT) ;  i++ ) {

			Irp = (((POGENINT_RW_CONTEXT) pCon) + i)->Irp;
			if (!Irp) continue;
			//
			// Since IoCallDriver has been called on this request, we call IoCancelIrp
			//  and let our completion routine handle it
			//
			cRes = IoCancelIrp( Irp );

			OGENINT_KdPrint ( DBGLVL_MAXIMUM,  ("OGENINT_CancelPendingIo() IoCancelIrp() cRes=%d, IRP 0x%x\n", cRes, Irp));

			// if cancel call failed, they all will, so dump out
			if ( !cRes ) break;

			uDriverCancel++; // flag we tried to cancel at least one

			// point to next context struct in array
			pCon +=  sizeof( OGENINT_RW_CONTEXT);
		} // end, for

		if ( uDriverCancel && cRes ) {

			// We only get here if we cancelled at least one and all cancellations were successfull.
			// Wait on the event set on last cancel in OGENINT_AsyncReadWriteComplete();
			OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("OGENINT_CancelPendingIo() before waiting for StagingDoneEvent()\n" ));
			waitStatus = KeWaitForSingleObject(
				&deviceExtension->StagingDoneEvent,
				Suspended,
				KernelMode,
				FALSE,
				NULL);
			OGENINT_KdPrint ( DBGLVL_MAXIMUM, ("OGENINT_CancelPendingIo() finished waiting for StagingDoneEvent()\n" ));
		}

		OGENINT_KdPrintCond ( DBGLVL_HIGH, uDriverCancel,
			("OGENINT_CancelPendingIo() cancelled %d via IoCancelIrp()\n",uDriverCancel));
	}

	return (BOOLEAN) uDriverCancel;                                  
}



NTSTATUS
OGENINT_AbortPipes(
    IN PDEVICE_OBJECT DeviceObject
    )
/*++

Routine Description:

	Called as part of sudden device removal handling.
    Cancels any pending transfers for all open pipes. 
	If any pipes are still open, call USBD with URB_FUNCTION_ABORT_PIPE
	Also marks the pipe 'closed' in our saved  configuration info.

Arguments:

    Ptrs to our FDO

Return Value:

    NT status code

--*/
{
	NTSTATUS ntStatus = STATUS_SUCCESS;
	PURB urb;
	PDEVICE_EXTENSION deviceExtension;
	ULONG i;

	PUSBD_INTERFACE_INFORMATION interface;
	PUSBD_PIPE_INFORMATION PipeInfo;

	deviceExtension = DeviceObject->DeviceExtension;
	interface = deviceExtension->UsbInterface;

	for (i=0; i<interface->NumberOfPipes; i++) {

		PipeInfo =  &interface->Pipes[i]; // PUSBD_PIPE_INFORMATION  PipeInfo;

		if ( PipeInfo->PipeFlags ) { // we set this if open, clear if closed

			OGENINT_KdPrint( DBGLVL_HIGH,("OGENINT_AbortPipes() Aborting open  Pipe %d\n", i));

			urb = OGENINT_ExAllocatePool(NonPagedPool,
					 sizeof(struct _URB_PIPE_REQUEST));

			if (urb) {

				urb->UrbHeader.Length = (USHORT) sizeof (struct _URB_PIPE_REQUEST);
				urb->UrbHeader.Function = URB_FUNCTION_ABORT_PIPE;
				urb->UrbPipeRequest.PipeHandle =
					PipeInfo->PipeHandle;

				ntStatus = OGENINT_CallUSBD(DeviceObject, urb);

				OGENINT_ExFreePool(urb);

			} else {
				ntStatus = STATUS_INSUFFICIENT_RESOURCES;
				OGENINT_KdPrint( DBGLVL_HIGH,("OGENINT_AbortPipes() FAILED urb alloc\n" ));
				break;
			}

			if (!(NT_SUCCESS(ntStatus))) {
				// if we failed, dump out
#if DBG
				if ( gpDbg ) gpDbg->PipeErrorCount++;
#endif
				break;
			} else {
				PipeInfo->PipeFlags = FALSE; // mark the pipe 'closed'

				deviceExtension->OpenPipeCount--;
#if DBG
				if ( gpDbg ) gpDbg->AbortPipeCount++;
#endif

			}

		} // end, if pipe open
	} // end, for all pipes

    return ntStatus;
}



BOOLEAN
OGENINT_CanAcceptIoRequests(
    IN PDEVICE_OBJECT DeviceObject
    )
/*++

Routine Description:

  Check device extension status flags; 

     Can't accept a new io request if device:
      1) is removed, 
      2) has never been started, 
      3) is stopped,
      4) has a remove request pending, or
      5) has a stop device pending


Arguments:

    DeviceObject - pointer to the device object for this instance of the 82930
                    device.


Return Value:

    return TRUE if can accept new io requests, else FALSE

--*/
{
	PDEVICE_EXTENSION deviceExtension;
	BOOLEAN fCan = FALSE;

	deviceExtension = DeviceObject->DeviceExtension;

	//flag set when processing IRP_MN_REMOVE_DEVICE
	if ( !deviceExtension->DeviceRemoved &&
		 // device must be started( enabled )
		 deviceExtension->DeviceStarted &&
 		 // flag set when driver has answered success to IRP_MN_QUERY_REMOVE_DEVICE
		 !deviceExtension->RemoveDeviceRequested &&
		 // flag set when driver has answered success to IRP_MN_QUERY_STOP_DEVICE
		 !deviceExtension->StopDeviceRequested )
	{
			fCan = TRUE;
	}

	OGENINT_KdPrintCond( DBGLVL_MAXIMUM, !fCan, ("**** FALSE return from OGENINT_CanAcceptIoRequests()!\n"));

	return fCan;
}

