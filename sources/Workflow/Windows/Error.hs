{-# LANGUAGE PatternSynonyms, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
module Workflow.Windows.Error where
import Workflow.Windows.Types (SystemErrorCode,getSystemErrorCode )

import Data.Maybe(fromMaybe) 

displaySystemErrorCode :: SystemErrorCode -> String
displaySystemErrorCode e = "SystemErrorCode " ++ fromMaybe (show (getSystemErrorCode e)) (displaySystemErrorCode' e)

displaySystemErrorCode' :: SystemErrorCode -> Maybe String
displaySystemErrorCode' ERROR_SUCCESS                                                            = Just "ERROR_SUCCESS"
displaySystemErrorCode' ERROR_INVALID_FUNCTION                                                   = Just "ERROR_INVALID_FUNCTION"
displaySystemErrorCode' ERROR_FILE_NOT_FOUND                                                     = Just "ERROR_FILE_NOT_FOUND"
displaySystemErrorCode' ERROR_PATH_NOT_FOUND                                                     = Just "ERROR_PATH_NOT_FOUND"
displaySystemErrorCode' ERROR_TOO_MANY_OPEN_FILES                                                = Just "ERROR_TOO_MANY_OPEN_FILES"
displaySystemErrorCode' ERROR_ACCESS_DENIED                                                      = Just "ERROR_ACCESS_DENIED"
displaySystemErrorCode' ERROR_INVALID_HANDLE                                                     = Just "ERROR_INVALID_HANDLE"
displaySystemErrorCode' ERROR_ARENA_TRASHED                                                      = Just "ERROR_ARENA_TRASHED"
displaySystemErrorCode' ERROR_NOT_ENOUGH_MEMORY                                                  = Just "ERROR_NOT_ENOUGH_MEMORY"
displaySystemErrorCode' ERROR_INVALID_BLOCK                                                      = Just "ERROR_INVALID_BLOCK"
displaySystemErrorCode' ERROR_BAD_ENVIRONMENT                                                    = Just "ERROR_BAD_ENVIRONMENT"
displaySystemErrorCode' ERROR_BAD_FORMAT                                                         = Just "ERROR_BAD_FORMAT"
displaySystemErrorCode' ERROR_INVALID_ACCESS                                                     = Just "ERROR_INVALID_ACCESS"
displaySystemErrorCode' ERROR_INVALID_DATA                                                       = Just "ERROR_INVALID_DATA"
displaySystemErrorCode' ERROR_OUTOFMEMORY                                                        = Just "ERROR_OUTOFMEMORY"
displaySystemErrorCode' ERROR_INVALID_DRIVE                                                      = Just "ERROR_INVALID_DRIVE"
displaySystemErrorCode' ERROR_CURRENT_DIRECTORY                                                  = Just "ERROR_CURRENT_DIRECTORY"
displaySystemErrorCode' ERROR_NOT_SAME_DEVICE                                                    = Just "ERROR_NOT_SAME_DEVICE"
displaySystemErrorCode' ERROR_NO_MORE_FILES                                                      = Just "ERROR_NO_MORE_FILES"
displaySystemErrorCode' ERROR_WRITE_PROTECT                                                      = Just "ERROR_WRITE_PROTECT"
displaySystemErrorCode' ERROR_BAD_UNIT                                                           = Just "ERROR_BAD_UNIT"
displaySystemErrorCode' ERROR_NOT_READY                                                          = Just "ERROR_NOT_READY"
displaySystemErrorCode' ERROR_BAD_COMMAND                                                        = Just "ERROR_BAD_COMMAND"
displaySystemErrorCode' ERROR_CRC                                                                = Just "ERROR_CRC"
displaySystemErrorCode' ERROR_BAD_LENGTH                                                         = Just "ERROR_BAD_LENGTH"
displaySystemErrorCode' ERROR_SEEK                                                               = Just "ERROR_SEEK"
displaySystemErrorCode' ERROR_NOT_DOS_DISK                                                       = Just "ERROR_NOT_DOS_DISK"
displaySystemErrorCode' ERROR_SECTOR_NOT_FOUND                                                   = Just "ERROR_SECTOR_NOT_FOUND"
displaySystemErrorCode' ERROR_OUT_OF_PAPER                                                       = Just "ERROR_OUT_OF_PAPER"
displaySystemErrorCode' ERROR_WRITE_FAULT                                                        = Just "ERROR_WRITE_FAULT"
displaySystemErrorCode' ERROR_READ_FAULT                                                         = Just "ERROR_READ_FAULT"
displaySystemErrorCode' ERROR_GEN_FAILURE                                                        = Just "ERROR_GEN_FAILURE"
displaySystemErrorCode' ERROR_SHARING_VIOLATION                                                  = Just "ERROR_SHARING_VIOLATION"
displaySystemErrorCode' ERROR_LOCK_VIOLATION                                                     = Just "ERROR_LOCK_VIOLATION"
displaySystemErrorCode' ERROR_WRONG_DISK                                                         = Just "ERROR_WRONG_DISK"
displaySystemErrorCode' ERROR_SHARING_BUFFER_EXCEEDED                                            = Just "ERROR_SHARING_BUFFER_EXCEEDED"
displaySystemErrorCode' ERROR_HANDLE_EOF                                                         = Just "ERROR_HANDLE_EOF"
displaySystemErrorCode' ERROR_HANDLE_DISK_FULL                                                   = Just "ERROR_HANDLE_DISK_FULL"
displaySystemErrorCode' ERROR_NOT_SUPPORTED                                                      = Just "ERROR_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_REM_NOT_LIST                                                       = Just "ERROR_REM_NOT_LIST"
displaySystemErrorCode' ERROR_DUP_NAME                                                           = Just "ERROR_DUP_NAME"
displaySystemErrorCode' ERROR_BAD_NETPATH                                                        = Just "ERROR_BAD_NETPATH"
displaySystemErrorCode' ERROR_NETWORK_BUSY                                                       = Just "ERROR_NETWORK_BUSY"
displaySystemErrorCode' ERROR_DEV_NOT_EXIST                                                      = Just "ERROR_DEV_NOT_EXIST"
displaySystemErrorCode' ERROR_TOO_MANY_CMDS                                                      = Just "ERROR_TOO_MANY_CMDS"
displaySystemErrorCode' ERROR_ADAP_HDW_ERR                                                       = Just "ERROR_ADAP_HDW_ERR"
displaySystemErrorCode' ERROR_BAD_NET_RESP                                                       = Just "ERROR_BAD_NET_RESP"
displaySystemErrorCode' ERROR_UNEXP_NET_ERR                                                      = Just "ERROR_UNEXP_NET_ERR"
displaySystemErrorCode' ERROR_BAD_REM_ADAP                                                       = Just "ERROR_BAD_REM_ADAP"
displaySystemErrorCode' ERROR_PRINTQ_FULL                                                        = Just "ERROR_PRINTQ_FULL"
displaySystemErrorCode' ERROR_NO_SPOOL_SPACE                                                     = Just "ERROR_NO_SPOOL_SPACE"
displaySystemErrorCode' ERROR_PRINT_CANCELLED                                                    = Just "ERROR_PRINT_CANCELLED"
displaySystemErrorCode' ERROR_NETNAME_DELETED                                                    = Just "ERROR_NETNAME_DELETED"
displaySystemErrorCode' ERROR_NETWORK_ACCESS_DENIED                                              = Just "ERROR_NETWORK_ACCESS_DENIED"
displaySystemErrorCode' ERROR_BAD_DEV_TYPE                                                       = Just "ERROR_BAD_DEV_TYPE"
displaySystemErrorCode' ERROR_BAD_NET_NAME                                                       = Just "ERROR_BAD_NET_NAME"
displaySystemErrorCode' ERROR_TOO_MANY_NAMES                                                     = Just "ERROR_TOO_MANY_NAMES"
displaySystemErrorCode' ERROR_TOO_MANY_SESS                                                      = Just "ERROR_TOO_MANY_SESS"
displaySystemErrorCode' ERROR_SHARING_PAUSED                                                     = Just "ERROR_SHARING_PAUSED"
displaySystemErrorCode' ERROR_REQ_NOT_ACCEP                                                      = Just "ERROR_REQ_NOT_ACCEP"
displaySystemErrorCode' ERROR_REDIR_PAUSED                                                       = Just "ERROR_REDIR_PAUSED"
displaySystemErrorCode' ERROR_FILE_EXISTS                                                        = Just "ERROR_FILE_EXISTS"
displaySystemErrorCode' ERROR_CANNOT_MAKE                                                        = Just "ERROR_CANNOT_MAKE"
displaySystemErrorCode' ERROR_FAIL_I24                                                           = Just "ERROR_FAIL_I24"
displaySystemErrorCode' ERROR_OUT_OF_STRUCTURES                                                  = Just "ERROR_OUT_OF_STRUCTURES"
displaySystemErrorCode' ERROR_ALREADY_ASSIGNED                                                   = Just "ERROR_ALREADY_ASSIGNED"
displaySystemErrorCode' ERROR_INVALID_PASSWORD                                                   = Just "ERROR_INVALID_PASSWORD"
displaySystemErrorCode' ERROR_INVALID_PARAMETER                                                  = Just "ERROR_INVALID_PARAMETER"
displaySystemErrorCode' ERROR_NET_WRITE_FAULT                                                    = Just "ERROR_NET_WRITE_FAULT"
displaySystemErrorCode' ERROR_NO_PROC_SLOTS                                                      = Just "ERROR_NO_PROC_SLOTS"
displaySystemErrorCode' ERROR_TOO_MANY_SEMAPHORES                                                = Just "ERROR_TOO_MANY_SEMAPHORES"
displaySystemErrorCode' ERROR_EXCL_SEM_ALREADY_OWNED                                             = Just "ERROR_EXCL_SEM_ALREADY_OWNED"
displaySystemErrorCode' ERROR_SEM_IS_SET                                                         = Just "ERROR_SEM_IS_SET"
displaySystemErrorCode' ERROR_TOO_MANY_SEM_REQUESTS                                              = Just "ERROR_TOO_MANY_SEM_REQUESTS"
displaySystemErrorCode' ERROR_INVALID_AT_INTERRUPT_TIME                                          = Just "ERROR_INVALID_AT_INTERRUPT_TIME"
displaySystemErrorCode' ERROR_SEM_OWNER_DIED                                                     = Just "ERROR_SEM_OWNER_DIED"
displaySystemErrorCode' ERROR_SEM_USER_LIMIT                                                     = Just "ERROR_SEM_USER_LIMIT"
displaySystemErrorCode' ERROR_DISK_CHANGE                                                        = Just "ERROR_DISK_CHANGE"
displaySystemErrorCode' ERROR_DRIVE_LOCKED                                                       = Just "ERROR_DRIVE_LOCKED"
displaySystemErrorCode' ERROR_BROKEN_PIPE                                                        = Just "ERROR_BROKEN_PIPE"
displaySystemErrorCode' ERROR_OPEN_FAILED                                                        = Just "ERROR_OPEN_FAILED"
displaySystemErrorCode' ERROR_BUFFER_OVERFLOW                                                    = Just "ERROR_BUFFER_OVERFLOW"
displaySystemErrorCode' ERROR_DISK_FULL                                                          = Just "ERROR_DISK_FULL"
displaySystemErrorCode' ERROR_NO_MORE_SEARCH_HANDLES                                             = Just "ERROR_NO_MORE_SEARCH_HANDLES"
displaySystemErrorCode' ERROR_INVALID_TARGET_HANDLE                                              = Just "ERROR_INVALID_TARGET_HANDLE"
displaySystemErrorCode' ERROR_INVALID_CATEGORY                                                   = Just "ERROR_INVALID_CATEGORY"
displaySystemErrorCode' ERROR_INVALID_VERIFY_SWITCH                                              = Just "ERROR_INVALID_VERIFY_SWITCH"
displaySystemErrorCode' ERROR_BAD_DRIVER_LEVEL                                                   = Just "ERROR_BAD_DRIVER_LEVEL"
displaySystemErrorCode' ERROR_CALL_NOT_IMPLEMENTED                                               = Just "ERROR_CALL_NOT_IMPLEMENTED"
displaySystemErrorCode' ERROR_SEM_TIMEOUT                                                        = Just "ERROR_SEM_TIMEOUT"
displaySystemErrorCode' ERROR_INSUFFICIENT_BUFFER                                                = Just "ERROR_INSUFFICIENT_BUFFER"
displaySystemErrorCode' ERROR_INVALID_NAME                                                       = Just "ERROR_INVALID_NAME"
displaySystemErrorCode' ERROR_INVALID_LEVEL                                                      = Just "ERROR_INVALID_LEVEL"
displaySystemErrorCode' ERROR_NO_VOLUME_LABEL                                                    = Just "ERROR_NO_VOLUME_LABEL"
displaySystemErrorCode' ERROR_MOD_NOT_FOUND                                                      = Just "ERROR_MOD_NOT_FOUND"
displaySystemErrorCode' ERROR_PROC_NOT_FOUND                                                     = Just "ERROR_PROC_NOT_FOUND"
displaySystemErrorCode' ERROR_WAIT_NO_CHILDREN                                                   = Just "ERROR_WAIT_NO_CHILDREN"
displaySystemErrorCode' ERROR_CHILD_NOT_COMPLETE                                                 = Just "ERROR_CHILD_NOT_COMPLETE"
displaySystemErrorCode' ERROR_DIRECT_ACCESS_HANDLE                                               = Just "ERROR_DIRECT_ACCESS_HANDLE"
displaySystemErrorCode' ERROR_NEGATIVE_SEEK                                                      = Just "ERROR_NEGATIVE_SEEK"
displaySystemErrorCode' ERROR_SEEK_ON_DEVICE                                                     = Just "ERROR_SEEK_ON_DEVICE"
displaySystemErrorCode' ERROR_IS_JOIN_TARGET                                                     = Just "ERROR_IS_JOIN_TARGET"
displaySystemErrorCode' ERROR_IS_JOINED                                                          = Just "ERROR_IS_JOINED"
displaySystemErrorCode' ERROR_IS_SUBSTED                                                         = Just "ERROR_IS_SUBSTED"
displaySystemErrorCode' ERROR_NOT_JOINED                                                         = Just "ERROR_NOT_JOINED"
displaySystemErrorCode' ERROR_NOT_SUBSTED                                                        = Just "ERROR_NOT_SUBSTED"
displaySystemErrorCode' ERROR_JOIN_TO_JOIN                                                       = Just "ERROR_JOIN_TO_JOIN"
displaySystemErrorCode' ERROR_SUBST_TO_SUBST                                                     = Just "ERROR_SUBST_TO_SUBST"
displaySystemErrorCode' ERROR_JOIN_TO_SUBST                                                      = Just "ERROR_JOIN_TO_SUBST"
displaySystemErrorCode' ERROR_SUBST_TO_JOIN                                                      = Just "ERROR_SUBST_TO_JOIN"
displaySystemErrorCode' ERROR_BUSY_DRIVE                                                         = Just "ERROR_BUSY_DRIVE"
displaySystemErrorCode' ERROR_SAME_DRIVE                                                         = Just "ERROR_SAME_DRIVE"
displaySystemErrorCode' ERROR_DIR_NOT_ROOT                                                       = Just "ERROR_DIR_NOT_ROOT"
displaySystemErrorCode' ERROR_DIR_NOT_EMPTY                                                      = Just "ERROR_DIR_NOT_EMPTY"
displaySystemErrorCode' ERROR_IS_SUBST_PATH                                                      = Just "ERROR_IS_SUBST_PATH"
displaySystemErrorCode' ERROR_IS_JOIN_PATH                                                       = Just "ERROR_IS_JOIN_PATH"
displaySystemErrorCode' ERROR_PATH_BUSY                                                          = Just "ERROR_PATH_BUSY"
displaySystemErrorCode' ERROR_IS_SUBST_TARGET                                                    = Just "ERROR_IS_SUBST_TARGET"
displaySystemErrorCode' ERROR_SYSTEM_TRACE                                                       = Just "ERROR_SYSTEM_TRACE"
displaySystemErrorCode' ERROR_INVALID_EVENT_COUNT                                                = Just "ERROR_INVALID_EVENT_COUNT"
displaySystemErrorCode' ERROR_TOO_MANY_MUXWAITERS                                                = Just "ERROR_TOO_MANY_MUXWAITERS"
displaySystemErrorCode' ERROR_INVALID_LIST_FORMAT                                                = Just "ERROR_INVALID_LIST_FORMAT"
displaySystemErrorCode' ERROR_LABEL_TOO_LONG                                                     = Just "ERROR_LABEL_TOO_LONG"
displaySystemErrorCode' ERROR_TOO_MANY_TCBS                                                      = Just "ERROR_TOO_MANY_TCBS"
displaySystemErrorCode' ERROR_SIGNAL_REFUSED                                                     = Just "ERROR_SIGNAL_REFUSED"
displaySystemErrorCode' ERROR_DISCARDED                                                          = Just "ERROR_DISCARDED"
displaySystemErrorCode' ERROR_NOT_LOCKED                                                         = Just "ERROR_NOT_LOCKED"
displaySystemErrorCode' ERROR_BAD_THREADID_ADDR                                                  = Just "ERROR_BAD_THREADID_ADDR"
displaySystemErrorCode' ERROR_BAD_ARGUMENTS                                                      = Just "ERROR_BAD_ARGUMENTS"
displaySystemErrorCode' ERROR_BAD_PATHNAME                                                       = Just "ERROR_BAD_PATHNAME"
displaySystemErrorCode' ERROR_SIGNAL_PENDING                                                     = Just "ERROR_SIGNAL_PENDING"
displaySystemErrorCode' ERROR_MAX_THRDS_REACHED                                                  = Just "ERROR_MAX_THRDS_REACHED"
displaySystemErrorCode' ERROR_LOCK_FAILED                                                        = Just "ERROR_LOCK_FAILED"
displaySystemErrorCode' ERROR_BUSY                                                               = Just "ERROR_BUSY"
displaySystemErrorCode' ERROR_DEVICE_SUPPORT_IN_PROGRESS                                         = Just "ERROR_DEVICE_SUPPORT_IN_PROGRESS"
displaySystemErrorCode' ERROR_CANCEL_VIOLATION                                                   = Just "ERROR_CANCEL_VIOLATION"
displaySystemErrorCode' ERROR_ATOMIC_LOCKS_NOT_SUPPORTED                                         = Just "ERROR_ATOMIC_LOCKS_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_INVALID_SEGMENT_NUMBER                                             = Just "ERROR_INVALID_SEGMENT_NUMBER"
displaySystemErrorCode' ERROR_INVALID_ORDINAL                                                    = Just "ERROR_INVALID_ORDINAL"
displaySystemErrorCode' ERROR_ALREADY_EXISTS                                                     = Just "ERROR_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_INVALID_FLAG_NUMBER                                                = Just "ERROR_INVALID_FLAG_NUMBER"
displaySystemErrorCode' ERROR_SEM_NOT_FOUND                                                      = Just "ERROR_SEM_NOT_FOUND"
displaySystemErrorCode' ERROR_INVALID_STARTING_CODESEG                                           = Just "ERROR_INVALID_STARTING_CODESEG"
displaySystemErrorCode' ERROR_INVALID_STACKSEG                                                   = Just "ERROR_INVALID_STACKSEG"
displaySystemErrorCode' ERROR_INVALID_MODULETYPE                                                 = Just "ERROR_INVALID_MODULETYPE"
displaySystemErrorCode' ERROR_INVALID_EXE_SIGNATURE                                              = Just "ERROR_INVALID_EXE_SIGNATURE"
displaySystemErrorCode' ERROR_EXE_MARKED_INVALID                                                 = Just "ERROR_EXE_MARKED_INVALID"
displaySystemErrorCode' ERROR_BAD_EXE_FORMAT                                                     = Just "ERROR_BAD_EXE_FORMAT"
displaySystemErrorCode' ERROR_ITERATED_DATA_EXCEEDS_64k                                          = Just "ERROR_ITERATED_DATA_EXCEEDS_64k"
displaySystemErrorCode' ERROR_INVALID_MINALLOCSIZE                                               = Just "ERROR_INVALID_MINALLOCSIZE"
displaySystemErrorCode' ERROR_DYNLINK_FROM_INVALID_RING                                          = Just "ERROR_DYNLINK_FROM_INVALID_RING"
displaySystemErrorCode' ERROR_IOPL_NOT_ENABLED                                                   = Just "ERROR_IOPL_NOT_ENABLED"
displaySystemErrorCode' ERROR_INVALID_SEGDPL                                                     = Just "ERROR_INVALID_SEGDPL"
displaySystemErrorCode' ERROR_AUTODATASEG_EXCEEDS_64k                                            = Just "ERROR_AUTODATASEG_EXCEEDS_64k"
displaySystemErrorCode' ERROR_RING2SEG_MUST_BE_MOVABLE                                           = Just "ERROR_RING2SEG_MUST_BE_MOVABLE"
displaySystemErrorCode' ERROR_RELOC_CHAIN_XEEDS_SEGLIM                                           = Just "ERROR_RELOC_CHAIN_XEEDS_SEGLIM"
displaySystemErrorCode' ERROR_INFLOOP_IN_RELOC_CHAIN                                             = Just "ERROR_INFLOOP_IN_RELOC_CHAIN"
displaySystemErrorCode' ERROR_ENVVAR_NOT_FOUND                                                   = Just "ERROR_ENVVAR_NOT_FOUND"
displaySystemErrorCode' ERROR_NO_SIGNAL_SENT                                                     = Just "ERROR_NO_SIGNAL_SENT"
displaySystemErrorCode' ERROR_FILENAME_EXCED_RANGE                                               = Just "ERROR_FILENAME_EXCED_RANGE"
displaySystemErrorCode' ERROR_RING2_STACK_IN_USE                                                 = Just "ERROR_RING2_STACK_IN_USE"
displaySystemErrorCode' ERROR_META_EXPANSION_TOO_LONG                                            = Just "ERROR_META_EXPANSION_TOO_LONG"
displaySystemErrorCode' ERROR_INVALID_SIGNAL_NUMBER                                              = Just "ERROR_INVALID_SIGNAL_NUMBER"
displaySystemErrorCode' ERROR_THREAD_1_INACTIVE                                                  = Just "ERROR_THREAD_1_INACTIVE"
displaySystemErrorCode' ERROR_LOCKED                                                             = Just "ERROR_LOCKED"
displaySystemErrorCode' ERROR_TOO_MANY_MODULES                                                   = Just "ERROR_TOO_MANY_MODULES"
displaySystemErrorCode' ERROR_NESTING_NOT_ALLOWED                                                = Just "ERROR_NESTING_NOT_ALLOWED"
displaySystemErrorCode' ERROR_EXE_MACHINE_TYPE_MISMATCH                                          = Just "ERROR_EXE_MACHINE_TYPE_MISMATCH"
displaySystemErrorCode' ERROR_EXE_CANNOT_MODIFY_SIGNED_BINARY                                    = Just "ERROR_EXE_CANNOT_MODIFY_SIGNED_BINARY"
displaySystemErrorCode' ERROR_EXE_CANNOT_MODIFY_STRONG_SIGNED_BINARY                             = Just "ERROR_EXE_CANNOT_MODIFY_STRONG_SIGNED_BINARY"
displaySystemErrorCode' ERROR_FILE_CHECKED_OUT                                                   = Just "ERROR_FILE_CHECKED_OUT"
displaySystemErrorCode' ERROR_CHECKOUT_REQUIRED                                                  = Just "ERROR_CHECKOUT_REQUIRED"
displaySystemErrorCode' ERROR_BAD_FILE_TYPE                                                      = Just "ERROR_BAD_FILE_TYPE"
displaySystemErrorCode' ERROR_FILE_TOO_LARGE                                                     = Just "ERROR_FILE_TOO_LARGE"
displaySystemErrorCode' ERROR_FORMS_AUTH_REQUIRED                                                = Just "ERROR_FORMS_AUTH_REQUIRED"
displaySystemErrorCode' ERROR_VIRUS_INFECTED                                                     = Just "ERROR_VIRUS_INFECTED"
displaySystemErrorCode' ERROR_VIRUS_DELETED                                                      = Just "ERROR_VIRUS_DELETED"
displaySystemErrorCode' ERROR_PIPE_LOCAL                                                         = Just "ERROR_PIPE_LOCAL"
displaySystemErrorCode' ERROR_BAD_PIPE                                                           = Just "ERROR_BAD_PIPE"
displaySystemErrorCode' ERROR_PIPE_BUSY                                                          = Just "ERROR_PIPE_BUSY"
displaySystemErrorCode' ERROR_NO_DATA                                                            = Just "ERROR_NO_DATA"
displaySystemErrorCode' ERROR_PIPE_NOT_CONNECTED                                                 = Just "ERROR_PIPE_NOT_CONNECTED"
displaySystemErrorCode' ERROR_MORE_DATA                                                          = Just "ERROR_MORE_DATA"
displaySystemErrorCode' ERROR_VC_DISCONNECTED                                                    = Just "ERROR_VC_DISCONNECTED"
displaySystemErrorCode' ERROR_INVALID_EA_NAME                                                    = Just "ERROR_INVALID_EA_NAME"
displaySystemErrorCode' ERROR_EA_LIST_INCONSISTENT                                               = Just "ERROR_EA_LIST_INCONSISTENT"
displaySystemErrorCode' ERROR_NO_MORE_ITEMS                                                      = Just "ERROR_NO_MORE_ITEMS"
displaySystemErrorCode' ERROR_CANNOT_COPY                                                        = Just "ERROR_CANNOT_COPY"
displaySystemErrorCode' ERROR_DIRECTORY                                                          = Just "ERROR_DIRECTORY"
displaySystemErrorCode' ERROR_EAS_DIDNT_FIT                                                      = Just "ERROR_EAS_DIDNT_FIT"
displaySystemErrorCode' ERROR_EA_FILE_CORRUPT                                                    = Just "ERROR_EA_FILE_CORRUPT"
displaySystemErrorCode' ERROR_EA_TABLE_FULL                                                      = Just "ERROR_EA_TABLE_FULL"
displaySystemErrorCode' ERROR_INVALID_EA_HANDLE                                                  = Just "ERROR_INVALID_EA_HANDLE"
displaySystemErrorCode' ERROR_EAS_NOT_SUPPORTED                                                  = Just "ERROR_EAS_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_NOT_OWNER                                                          = Just "ERROR_NOT_OWNER"
displaySystemErrorCode' ERROR_TOO_MANY_POSTS                                                     = Just "ERROR_TOO_MANY_POSTS"
displaySystemErrorCode' ERROR_PARTIAL_COPY                                                       = Just "ERROR_PARTIAL_COPY"
displaySystemErrorCode' ERROR_OPLOCK_NOT_GRANTED                                                 = Just "ERROR_OPLOCK_NOT_GRANTED"
displaySystemErrorCode' ERROR_INVALID_OPLOCK_PROTOCOL                                            = Just "ERROR_INVALID_OPLOCK_PROTOCOL"
displaySystemErrorCode' ERROR_DISK_TOO_FRAGMENTED                                                = Just "ERROR_DISK_TOO_FRAGMENTED"
displaySystemErrorCode' ERROR_DELETE_PENDING                                                     = Just "ERROR_DELETE_PENDING"
displaySystemErrorCode' ERROR_INCOMPATIBLE_WITH_GLOBAL_SHORT_NAME_REGISTRY_SETTING               = Just "ERROR_INCOMPATIBLE_WITH_GLOBAL_SHORT_NAME_REGISTRY_SETTING"
displaySystemErrorCode' ERROR_SHORT_NAMES_NOT_ENABLED_ON_VOLUME                                  = Just "ERROR_SHORT_NAMES_NOT_ENABLED_ON_VOLUME"
displaySystemErrorCode' ERROR_SECURITY_STREAM_IS_INCONSISTENT                                    = Just "ERROR_SECURITY_STREAM_IS_INCONSISTENT"
displaySystemErrorCode' ERROR_INVALID_LOCK_RANGE                                                 = Just "ERROR_INVALID_LOCK_RANGE"
displaySystemErrorCode' ERROR_IMAGE_SUBSYSTEM_NOT_PRESENT                                        = Just "ERROR_IMAGE_SUBSYSTEM_NOT_PRESENT"
displaySystemErrorCode' ERROR_NOTIFICATION_GUID_ALREADY_DEFINED                                  = Just "ERROR_NOTIFICATION_GUID_ALREADY_DEFINED"
displaySystemErrorCode' ERROR_INVALID_EXCEPTION_HANDLER                                          = Just "ERROR_INVALID_EXCEPTION_HANDLER"
displaySystemErrorCode' ERROR_DUPLICATE_PRIVILEGES                                               = Just "ERROR_DUPLICATE_PRIVILEGES"
displaySystemErrorCode' ERROR_NO_RANGES_PROCESSED                                                = Just "ERROR_NO_RANGES_PROCESSED"
displaySystemErrorCode' ERROR_NOT_ALLOWED_ON_SYSTEM_FILE                                         = Just "ERROR_NOT_ALLOWED_ON_SYSTEM_FILE"
displaySystemErrorCode' ERROR_DISK_RESOURCES_EXHAUSTED                                           = Just "ERROR_DISK_RESOURCES_EXHAUSTED"
displaySystemErrorCode' ERROR_INVALID_TOKEN                                                      = Just "ERROR_INVALID_TOKEN"
displaySystemErrorCode' ERROR_DEVICE_FEATURE_NOT_SUPPORTED                                       = Just "ERROR_DEVICE_FEATURE_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_MR_MID_NOT_FOUND                                                   = Just "ERROR_MR_MID_NOT_FOUND"
displaySystemErrorCode' ERROR_SCOPE_NOT_FOUND                                                    = Just "ERROR_SCOPE_NOT_FOUND"
displaySystemErrorCode' ERROR_UNDEFINED_SCOPE                                                    = Just "ERROR_UNDEFINED_SCOPE"
displaySystemErrorCode' ERROR_INVALID_CAP                                                        = Just "ERROR_INVALID_CAP"
displaySystemErrorCode' ERROR_DEVICE_UNREACHABLE                                                 = Just "ERROR_DEVICE_UNREACHABLE"
displaySystemErrorCode' ERROR_DEVICE_NO_RESOURCES                                                = Just "ERROR_DEVICE_NO_RESOURCES"
displaySystemErrorCode' ERROR_DATA_CHECKSUM_ERROR                                                = Just "ERROR_DATA_CHECKSUM_ERROR"
displaySystemErrorCode' ERROR_INTERMIXED_KERNEL_EA_OPERATION                                     = Just "ERROR_INTERMIXED_KERNEL_EA_OPERATION"
displaySystemErrorCode' ERROR_FILE_LEVEL_TRIM_NOT_SUPPORTED                                      = Just "ERROR_FILE_LEVEL_TRIM_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_OFFSET_ALIGNMENT_VIOLATION                                         = Just "ERROR_OFFSET_ALIGNMENT_VIOLATION"
displaySystemErrorCode' ERROR_INVALID_FIELD_IN_PARAMETER_LIST                                    = Just "ERROR_INVALID_FIELD_IN_PARAMETER_LIST"
displaySystemErrorCode' ERROR_OPERATION_IN_PROGRESS                                              = Just "ERROR_OPERATION_IN_PROGRESS"
displaySystemErrorCode' ERROR_BAD_DEVICE_PATH                                                    = Just "ERROR_BAD_DEVICE_PATH"
displaySystemErrorCode' ERROR_TOO_MANY_DESCRIPTORS                                               = Just "ERROR_TOO_MANY_DESCRIPTORS"
displaySystemErrorCode' ERROR_SCRUB_DATA_DISABLED                                                = Just "ERROR_SCRUB_DATA_DISABLED"
displaySystemErrorCode' ERROR_NOT_REDUNDANT_STORAGE                                              = Just "ERROR_NOT_REDUNDANT_STORAGE"
displaySystemErrorCode' ERROR_RESIDENT_FILE_NOT_SUPPORTED                                        = Just "ERROR_RESIDENT_FILE_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_COMPRESSED_FILE_NOT_SUPPORTED                                      = Just "ERROR_COMPRESSED_FILE_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_DIRECTORY_NOT_SUPPORTED                                            = Just "ERROR_DIRECTORY_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_NOT_READ_FROM_COPY                                                 = Just "ERROR_NOT_READ_FROM_COPY"
displaySystemErrorCode' ERROR_FT_WRITE_FAILURE                                                   = Just "ERROR_FT_WRITE_FAILURE"
displaySystemErrorCode' ERROR_FT_DI_SCAN_REQUIRED                                                = Just "ERROR_FT_DI_SCAN_REQUIRED"
displaySystemErrorCode' ERROR_INVALID_KERNEL_INFO_VERSION                                        = Just "ERROR_INVALID_KERNEL_INFO_VERSION"
displaySystemErrorCode' ERROR_INVALID_PEP_INFO_VERSION                                           = Just "ERROR_INVALID_PEP_INFO_VERSION"
displaySystemErrorCode' ERROR_OBJECT_NOT_EXTERNALLY_BACKED                                       = Just "ERROR_OBJECT_NOT_EXTERNALLY_BACKED"
displaySystemErrorCode' ERROR_EXTERNAL_BACKING_PROVIDER_UNKNOWN                                  = Just "ERROR_EXTERNAL_BACKING_PROVIDER_UNKNOWN"
displaySystemErrorCode' ERROR_FAIL_NOACTION_REBOOT                                               = Just "ERROR_FAIL_NOACTION_REBOOT"
displaySystemErrorCode' ERROR_FAIL_SHUTDOWN                                                      = Just "ERROR_FAIL_SHUTDOWN"
displaySystemErrorCode' ERROR_FAIL_RESTART                                                       = Just "ERROR_FAIL_RESTART"
displaySystemErrorCode' ERROR_MAX_SESSIONS_REACHED                                               = Just "ERROR_MAX_SESSIONS_REACHED"
displaySystemErrorCode' ERROR_THREAD_MODE_ALREADY_BACKGROUND                                     = Just "ERROR_THREAD_MODE_ALREADY_BACKGROUND"
displaySystemErrorCode' ERROR_THREAD_MODE_NOT_BACKGROUND                                         = Just "ERROR_THREAD_MODE_NOT_BACKGROUND"
displaySystemErrorCode' ERROR_PROCESS_MODE_ALREADY_BACKGROUND                                    = Just "ERROR_PROCESS_MODE_ALREADY_BACKGROUND"
displaySystemErrorCode' ERROR_PROCESS_MODE_NOT_BACKGROUND                                        = Just "ERROR_PROCESS_MODE_NOT_BACKGROUND"
displaySystemErrorCode' ERROR_DEVICE_HARDWARE_ERROR                                              = Just "ERROR_DEVICE_HARDWARE_ERROR"
displaySystemErrorCode' ERROR_INVALID_ADDRESS                                                    = Just "ERROR_INVALID_ADDRESS"
displaySystemErrorCode' ERROR_USER_PROFILE_LOAD                                                  = Just "ERROR_USER_PROFILE_LOAD"
displaySystemErrorCode' ERROR_ARITHMETIC_OVERFLOW                                                = Just "ERROR_ARITHMETIC_OVERFLOW"
displaySystemErrorCode' ERROR_PIPE_CONNECTED                                                     = Just "ERROR_PIPE_CONNECTED"
displaySystemErrorCode' ERROR_PIPE_LISTENING                                                     = Just "ERROR_PIPE_LISTENING"
displaySystemErrorCode' ERROR_VERIFIER_STOP                                                      = Just "ERROR_VERIFIER_STOP"
displaySystemErrorCode' ERROR_ABIOS_ERROR                                                        = Just "ERROR_ABIOS_ERROR"
displaySystemErrorCode' ERROR_WX86_WARNING                                                       = Just "ERROR_WX86_WARNING"
displaySystemErrorCode' ERROR_WX86_ERROR                                                         = Just "ERROR_WX86_ERROR"
displaySystemErrorCode' ERROR_TIMER_NOT_CANCELED                                                 = Just "ERROR_TIMER_NOT_CANCELED"
displaySystemErrorCode' ERROR_UNWIND                                                             = Just "ERROR_UNWIND"
displaySystemErrorCode' ERROR_BAD_STACK                                                          = Just "ERROR_BAD_STACK"
displaySystemErrorCode' ERROR_INVALID_UNWIND_TARGET                                              = Just "ERROR_INVALID_UNWIND_TARGET"
displaySystemErrorCode' ERROR_INVALID_PORT_ATTRIBUTES                                            = Just "ERROR_INVALID_PORT_ATTRIBUTES"
displaySystemErrorCode' ERROR_PORT_MESSAGE_TOO_LONG                                              = Just "ERROR_PORT_MESSAGE_TOO_LONG"
displaySystemErrorCode' ERROR_INVALID_QUOTA_LOWER                                                = Just "ERROR_INVALID_QUOTA_LOWER"
displaySystemErrorCode' ERROR_DEVICE_ALREADY_ATTACHED                                            = Just "ERROR_DEVICE_ALREADY_ATTACHED"
displaySystemErrorCode' ERROR_INSTRUCTION_MISALIGNMENT                                           = Just "ERROR_INSTRUCTION_MISALIGNMENT"
displaySystemErrorCode' ERROR_PROFILING_NOT_STARTED                                              = Just "ERROR_PROFILING_NOT_STARTED"
displaySystemErrorCode' ERROR_PROFILING_NOT_STOPPED                                              = Just "ERROR_PROFILING_NOT_STOPPED"
displaySystemErrorCode' ERROR_COULD_NOT_INTERPRET                                                = Just "ERROR_COULD_NOT_INTERPRET"
displaySystemErrorCode' ERROR_PROFILING_AT_LIMIT                                                 = Just "ERROR_PROFILING_AT_LIMIT"
displaySystemErrorCode' ERROR_CANT_WAIT                                                          = Just "ERROR_CANT_WAIT"
displaySystemErrorCode' ERROR_CANT_TERMINATE_SELF                                                = Just "ERROR_CANT_TERMINATE_SELF"
displaySystemErrorCode' ERROR_UNEXPECTED_MM_CREATE_ERR                                           = Just "ERROR_UNEXPECTED_MM_CREATE_ERR"
displaySystemErrorCode' ERROR_UNEXPECTED_MM_MAP_ERROR                                            = Just "ERROR_UNEXPECTED_MM_MAP_ERROR"
displaySystemErrorCode' ERROR_UNEXPECTED_MM_EXTEND_ERR                                           = Just "ERROR_UNEXPECTED_MM_EXTEND_ERR"
displaySystemErrorCode' ERROR_BAD_FUNCTION_TABLE                                                 = Just "ERROR_BAD_FUNCTION_TABLE"
displaySystemErrorCode' ERROR_NO_GUID_TRANSLATION                                                = Just "ERROR_NO_GUID_TRANSLATION"
displaySystemErrorCode' ERROR_INVALID_LDT_SIZE                                                   = Just "ERROR_INVALID_LDT_SIZE"
displaySystemErrorCode' ERROR_INVALID_LDT_OFFSET                                                 = Just "ERROR_INVALID_LDT_OFFSET"
displaySystemErrorCode' ERROR_INVALID_LDT_DESCRIPTOR                                             = Just "ERROR_INVALID_LDT_DESCRIPTOR"
displaySystemErrorCode' ERROR_TOO_MANY_THREADS                                                   = Just "ERROR_TOO_MANY_THREADS"
displaySystemErrorCode' ERROR_THREAD_NOT_IN_PROCESS                                              = Just "ERROR_THREAD_NOT_IN_PROCESS"
displaySystemErrorCode' ERROR_PAGEFILE_QUOTA_EXCEEDED                                            = Just "ERROR_PAGEFILE_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_LOGON_SERVER_CONFLICT                                              = Just "ERROR_LOGON_SERVER_CONFLICT"
displaySystemErrorCode' ERROR_SYNCHRONIZATION_REQUIRED                                           = Just "ERROR_SYNCHRONIZATION_REQUIRED"
displaySystemErrorCode' ERROR_NET_OPEN_FAILED                                                    = Just "ERROR_NET_OPEN_FAILED"
displaySystemErrorCode' ERROR_IO_PRIVILEGE_FAILED                                                = Just "ERROR_IO_PRIVILEGE_FAILED"
displaySystemErrorCode' ERROR_CONTROL_C_EXIT                                                     = Just "ERROR_CONTROL_C_EXIT"
displaySystemErrorCode' ERROR_MISSING_SYSTEMFILE                                                 = Just "ERROR_MISSING_SYSTEMFILE"
displaySystemErrorCode' ERROR_UNHANDLED_EXCEPTION                                                = Just "ERROR_UNHANDLED_EXCEPTION"
displaySystemErrorCode' ERROR_APP_INIT_FAILURE                                                   = Just "ERROR_APP_INIT_FAILURE"
displaySystemErrorCode' ERROR_PAGEFILE_CREATE_FAILED                                             = Just "ERROR_PAGEFILE_CREATE_FAILED"
displaySystemErrorCode' ERROR_INVALID_IMAGE_HASH                                                 = Just "ERROR_INVALID_IMAGE_HASH"
displaySystemErrorCode' ERROR_NO_PAGEFILE                                                        = Just "ERROR_NO_PAGEFILE"
displaySystemErrorCode' ERROR_ILLEGAL_FLOAT_CONTEXT                                              = Just "ERROR_ILLEGAL_FLOAT_CONTEXT"
displaySystemErrorCode' ERROR_NO_EVENT_PAIR                                                      = Just "ERROR_NO_EVENT_PAIR"
displaySystemErrorCode' ERROR_DOMAIN_CTRLR_CONFIG_ERROR                                          = Just "ERROR_DOMAIN_CTRLR_CONFIG_ERROR"
displaySystemErrorCode' ERROR_ILLEGAL_CHARACTER                                                  = Just "ERROR_ILLEGAL_CHARACTER"
displaySystemErrorCode' ERROR_UNDEFINED_CHARACTER                                                = Just "ERROR_UNDEFINED_CHARACTER"
displaySystemErrorCode' ERROR_FLOPPY_VOLUME                                                      = Just "ERROR_FLOPPY_VOLUME"
displaySystemErrorCode' ERROR_BIOS_FAILED_TO_CONNECT_INTERRUPT                                   = Just "ERROR_BIOS_FAILED_TO_CONNECT_INTERRUPT"
displaySystemErrorCode' ERROR_BACKUP_CONTROLLER                                                  = Just "ERROR_BACKUP_CONTROLLER"
displaySystemErrorCode' ERROR_MUTANT_LIMIT_EXCEEDED                                              = Just "ERROR_MUTANT_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_FS_DRIVER_REQUIRED                                                 = Just "ERROR_FS_DRIVER_REQUIRED"
displaySystemErrorCode' ERROR_CANNOT_LOAD_REGISTRY_FILE                                          = Just "ERROR_CANNOT_LOAD_REGISTRY_FILE"
displaySystemErrorCode' ERROR_DEBUG_ATTACH_FAILED                                                = Just "ERROR_DEBUG_ATTACH_FAILED"
displaySystemErrorCode' ERROR_SYSTEM_PROCESS_TERMINATED                                          = Just "ERROR_SYSTEM_PROCESS_TERMINATED"
displaySystemErrorCode' ERROR_DATA_NOT_ACCEPTED                                                  = Just "ERROR_DATA_NOT_ACCEPTED"
displaySystemErrorCode' ERROR_VDM_HARD_ERROR                                                     = Just "ERROR_VDM_HARD_ERROR"
displaySystemErrorCode' ERROR_DRIVER_CANCEL_TIMEOUT                                              = Just "ERROR_DRIVER_CANCEL_TIMEOUT"
displaySystemErrorCode' ERROR_REPLY_MESSAGE_MISMATCH                                             = Just "ERROR_REPLY_MESSAGE_MISMATCH"
displaySystemErrorCode' ERROR_LOST_WRITEBEHIND_DATA                                              = Just "ERROR_LOST_WRITEBEHIND_DATA"
displaySystemErrorCode' ERROR_CLIENT_SERVER_PARAMETERS_INVALID                                   = Just "ERROR_CLIENT_SERVER_PARAMETERS_INVALID"
displaySystemErrorCode' ERROR_NOT_TINY_STREAM                                                    = Just "ERROR_NOT_TINY_STREAM"
displaySystemErrorCode' ERROR_STACK_OVERFLOW_READ                                                = Just "ERROR_STACK_OVERFLOW_READ"
displaySystemErrorCode' ERROR_CONVERT_TO_LARGE                                                   = Just "ERROR_CONVERT_TO_LARGE"
displaySystemErrorCode' ERROR_FOUND_OUT_OF_SCOPE                                                 = Just "ERROR_FOUND_OUT_OF_SCOPE"
displaySystemErrorCode' ERROR_ALLOCATE_BUCKET                                                    = Just "ERROR_ALLOCATE_BUCKET"
displaySystemErrorCode' ERROR_MARSHALL_OVERFLOW                                                  = Just "ERROR_MARSHALL_OVERFLOW"
displaySystemErrorCode' ERROR_INVALID_VARIANT                                                    = Just "ERROR_INVALID_VARIANT"
displaySystemErrorCode' ERROR_BAD_COMPRESSION_BUFFER                                             = Just "ERROR_BAD_COMPRESSION_BUFFER"
displaySystemErrorCode' ERROR_AUDIT_FAILED                                                       = Just "ERROR_AUDIT_FAILED"
displaySystemErrorCode' ERROR_TIMER_RESOLUTION_NOT_SET                                           = Just "ERROR_TIMER_RESOLUTION_NOT_SET"
displaySystemErrorCode' ERROR_INSUFFICIENT_LOGON_INFO                                            = Just "ERROR_INSUFFICIENT_LOGON_INFO"
displaySystemErrorCode' ERROR_BAD_DLL_ENTRYPOINT                                                 = Just "ERROR_BAD_DLL_ENTRYPOINT"
displaySystemErrorCode' ERROR_BAD_SERVICE_ENTRYPOINT                                             = Just "ERROR_BAD_SERVICE_ENTRYPOINT"
displaySystemErrorCode' ERROR_IP_ADDRESS_CONFLICT1                                               = Just "ERROR_IP_ADDRESS_CONFLICT1"
displaySystemErrorCode' ERROR_IP_ADDRESS_CONFLICT2                                               = Just "ERROR_IP_ADDRESS_CONFLICT2"
displaySystemErrorCode' ERROR_REGISTRY_QUOTA_LIMIT                                               = Just "ERROR_REGISTRY_QUOTA_LIMIT"
displaySystemErrorCode' ERROR_NO_CALLBACK_ACTIVE                                                 = Just "ERROR_NO_CALLBACK_ACTIVE"
displaySystemErrorCode' ERROR_PWD_TOO_SHORT                                                      = Just "ERROR_PWD_TOO_SHORT"
displaySystemErrorCode' ERROR_PWD_TOO_RECENT                                                     = Just "ERROR_PWD_TOO_RECENT"
displaySystemErrorCode' ERROR_PWD_HISTORY_CONFLICT                                               = Just "ERROR_PWD_HISTORY_CONFLICT"
displaySystemErrorCode' ERROR_UNSUPPORTED_COMPRESSION                                            = Just "ERROR_UNSUPPORTED_COMPRESSION"
displaySystemErrorCode' ERROR_INVALID_HW_PROFILE                                                 = Just "ERROR_INVALID_HW_PROFILE"
displaySystemErrorCode' ERROR_INVALID_PLUGPLAY_DEVICE_PATH                                       = Just "ERROR_INVALID_PLUGPLAY_DEVICE_PATH"
displaySystemErrorCode' ERROR_QUOTA_LIST_INCONSISTENT                                            = Just "ERROR_QUOTA_LIST_INCONSISTENT"
displaySystemErrorCode' ERROR_EVALUATION_EXPIRATION                                              = Just "ERROR_EVALUATION_EXPIRATION"
displaySystemErrorCode' ERROR_ILLEGAL_DLL_RELOCATION                                             = Just "ERROR_ILLEGAL_DLL_RELOCATION"
displaySystemErrorCode' ERROR_DLL_INIT_FAILED_LOGOFF                                             = Just "ERROR_DLL_INIT_FAILED_LOGOFF"
displaySystemErrorCode' ERROR_VALIDATE_CONTINUE                                                  = Just "ERROR_VALIDATE_CONTINUE"
displaySystemErrorCode' ERROR_NO_MORE_MATCHES                                                    = Just "ERROR_NO_MORE_MATCHES"
displaySystemErrorCode' ERROR_RANGE_LIST_CONFLICT                                                = Just "ERROR_RANGE_LIST_CONFLICT"
displaySystemErrorCode' ERROR_SERVER_SID_MISMATCH                                                = Just "ERROR_SERVER_SID_MISMATCH"
displaySystemErrorCode' ERROR_CANT_ENABLE_DENY_ONLY                                              = Just "ERROR_CANT_ENABLE_DENY_ONLY"
displaySystemErrorCode' ERROR_FLOAT_MULTIPLE_FAULTS                                              = Just "ERROR_FLOAT_MULTIPLE_FAULTS"
displaySystemErrorCode' ERROR_FLOAT_MULTIPLE_TRAPS                                               = Just "ERROR_FLOAT_MULTIPLE_TRAPS"
displaySystemErrorCode' ERROR_NOINTERFACE                                                        = Just "ERROR_NOINTERFACE"
displaySystemErrorCode' ERROR_DRIVER_FAILED_SLEEP                                                = Just "ERROR_DRIVER_FAILED_SLEEP"
displaySystemErrorCode' ERROR_CORRUPT_SYSTEM_FILE                                                = Just "ERROR_CORRUPT_SYSTEM_FILE"
displaySystemErrorCode' ERROR_COMMITMENT_MINIMUM                                                 = Just "ERROR_COMMITMENT_MINIMUM"
displaySystemErrorCode' ERROR_PNP_RESTART_ENUMERATION                                            = Just "ERROR_PNP_RESTART_ENUMERATION"
displaySystemErrorCode' ERROR_SYSTEM_IMAGE_BAD_SIGNATURE                                         = Just "ERROR_SYSTEM_IMAGE_BAD_SIGNATURE"
displaySystemErrorCode' ERROR_PNP_REBOOT_REQUIRED                                                = Just "ERROR_PNP_REBOOT_REQUIRED"
displaySystemErrorCode' ERROR_INSUFFICIENT_POWER                                                 = Just "ERROR_INSUFFICIENT_POWER"
displaySystemErrorCode' ERROR_MULTIPLE_FAULT_VIOLATION                                           = Just "ERROR_MULTIPLE_FAULT_VIOLATION"
displaySystemErrorCode' ERROR_SYSTEM_SHUTDOWN                                                    = Just "ERROR_SYSTEM_SHUTDOWN"
displaySystemErrorCode' ERROR_PORT_NOT_SET                                                       = Just "ERROR_PORT_NOT_SET"
displaySystemErrorCode' ERROR_DS_VERSION_CHECK_FAILURE                                           = Just "ERROR_DS_VERSION_CHECK_FAILURE"
displaySystemErrorCode' ERROR_RANGE_NOT_FOUND                                                    = Just "ERROR_RANGE_NOT_FOUND"
displaySystemErrorCode' ERROR_NOT_SAFE_MODE_DRIVER                                               = Just "ERROR_NOT_SAFE_MODE_DRIVER"
displaySystemErrorCode' ERROR_FAILED_DRIVER_ENTRY                                                = Just "ERROR_FAILED_DRIVER_ENTRY"
displaySystemErrorCode' ERROR_DEVICE_ENUMERATION_ERROR                                           = Just "ERROR_DEVICE_ENUMERATION_ERROR"
displaySystemErrorCode' ERROR_MOUNT_POINT_NOT_RESOLVED                                           = Just "ERROR_MOUNT_POINT_NOT_RESOLVED"
displaySystemErrorCode' ERROR_INVALID_DEVICE_OBJECT_PARAMETER                                    = Just "ERROR_INVALID_DEVICE_OBJECT_PARAMETER"
displaySystemErrorCode' ERROR_MCA_OCCURED                                                        = Just "ERROR_MCA_OCCURED"
displaySystemErrorCode' ERROR_DRIVER_DATABASE_ERROR                                              = Just "ERROR_DRIVER_DATABASE_ERROR"
displaySystemErrorCode' ERROR_SYSTEM_HIVE_TOO_LARGE                                              = Just "ERROR_SYSTEM_HIVE_TOO_LARGE"
displaySystemErrorCode' ERROR_DRIVER_FAILED_PRIOR_UNLOAD                                         = Just "ERROR_DRIVER_FAILED_PRIOR_UNLOAD"
displaySystemErrorCode' ERROR_VOLSNAP_PREPARE_HIBERNATE                                          = Just "ERROR_VOLSNAP_PREPARE_HIBERNATE"
displaySystemErrorCode' ERROR_HIBERNATION_FAILURE                                                = Just "ERROR_HIBERNATION_FAILURE"
displaySystemErrorCode' ERROR_PWD_TOO_LONG                                                       = Just "ERROR_PWD_TOO_LONG"
displaySystemErrorCode' ERROR_FILE_SYSTEM_LIMITATION                                             = Just "ERROR_FILE_SYSTEM_LIMITATION"
displaySystemErrorCode' ERROR_ASSERTION_FAILURE                                                  = Just "ERROR_ASSERTION_FAILURE"
displaySystemErrorCode' ERROR_ACPI_ERROR                                                         = Just "ERROR_ACPI_ERROR"
displaySystemErrorCode' ERROR_WOW_ASSERTION                                                      = Just "ERROR_WOW_ASSERTION"
displaySystemErrorCode' ERROR_PNP_BAD_MPS_TABLE                                                  = Just "ERROR_PNP_BAD_MPS_TABLE"
displaySystemErrorCode' ERROR_PNP_TRANSLATION_FAILED                                             = Just "ERROR_PNP_TRANSLATION_FAILED"
displaySystemErrorCode' ERROR_PNP_IRQ_TRANSLATION_FAILED                                         = Just "ERROR_PNP_IRQ_TRANSLATION_FAILED"
displaySystemErrorCode' ERROR_PNP_INVALID_ID                                                     = Just "ERROR_PNP_INVALID_ID"
displaySystemErrorCode' ERROR_WAKE_SYSTEM_DEBUGGER                                               = Just "ERROR_WAKE_SYSTEM_DEBUGGER"
displaySystemErrorCode' ERROR_HANDLES_CLOSED                                                     = Just "ERROR_HANDLES_CLOSED"
displaySystemErrorCode' ERROR_EXTRANEOUS_INFORMATION                                             = Just "ERROR_EXTRANEOUS_INFORMATION"
displaySystemErrorCode' ERROR_RXACT_COMMIT_NECESSARY                                             = Just "ERROR_RXACT_COMMIT_NECESSARY"
displaySystemErrorCode' ERROR_MEDIA_CHECK                                                        = Just "ERROR_MEDIA_CHECK"
displaySystemErrorCode' ERROR_GUID_SUBSTITUTION_MADE                                             = Just "ERROR_GUID_SUBSTITUTION_MADE"
displaySystemErrorCode' ERROR_STOPPED_ON_SYMLINK                                                 = Just "ERROR_STOPPED_ON_SYMLINK"
displaySystemErrorCode' ERROR_LONGJUMP                                                           = Just "ERROR_LONGJUMP"
displaySystemErrorCode' ERROR_PLUGPLAY_QUERY_VETOED                                              = Just "ERROR_PLUGPLAY_QUERY_VETOED"
displaySystemErrorCode' ERROR_UNWIND_CONSOLIDATE                                                 = Just "ERROR_UNWIND_CONSOLIDATE"
displaySystemErrorCode' ERROR_REGISTRY_HIVE_RECOVERED                                            = Just "ERROR_REGISTRY_HIVE_RECOVERED"
displaySystemErrorCode' ERROR_DLL_MIGHT_BE_INSECURE                                              = Just "ERROR_DLL_MIGHT_BE_INSECURE"
displaySystemErrorCode' ERROR_DLL_MIGHT_BE_INCOMPATIBLE                                          = Just "ERROR_DLL_MIGHT_BE_INCOMPATIBLE"
displaySystemErrorCode' ERROR_DBG_EXCEPTION_NOT_HANDLED                                          = Just "ERROR_DBG_EXCEPTION_NOT_HANDLED"
displaySystemErrorCode' ERROR_DBG_REPLY_LATER                                                    = Just "ERROR_DBG_REPLY_LATER"
displaySystemErrorCode' ERROR_DBG_UNABLE_TO_PROVIDE_HANDLE                                       = Just "ERROR_DBG_UNABLE_TO_PROVIDE_HANDLE"
displaySystemErrorCode' ERROR_DBG_TERMINATE_THREAD                                               = Just "ERROR_DBG_TERMINATE_THREAD"
displaySystemErrorCode' ERROR_DBG_TERMINATE_PROCESS                                              = Just "ERROR_DBG_TERMINATE_PROCESS"
displaySystemErrorCode' ERROR_DBG_CONTROL_C                                                      = Just "ERROR_DBG_CONTROL_C"
displaySystemErrorCode' ERROR_DBG_PRINTEXCEPTION_C                                               = Just "ERROR_DBG_PRINTEXCEPTION_C"
displaySystemErrorCode' ERROR_DBG_RIPEXCEPTION                                                   = Just "ERROR_DBG_RIPEXCEPTION"
displaySystemErrorCode' ERROR_DBG_CONTROL_BREAK                                                  = Just "ERROR_DBG_CONTROL_BREAK"
displaySystemErrorCode' ERROR_DBG_COMMAND_EXCEPTION                                              = Just "ERROR_DBG_COMMAND_EXCEPTION"
displaySystemErrorCode' ERROR_OBJECT_NAME_EXISTS                                                 = Just "ERROR_OBJECT_NAME_EXISTS"
displaySystemErrorCode' ERROR_THREAD_WAS_SUSPENDED                                               = Just "ERROR_THREAD_WAS_SUSPENDED"
displaySystemErrorCode' ERROR_IMAGE_NOT_AT_BASE                                                  = Just "ERROR_IMAGE_NOT_AT_BASE"
displaySystemErrorCode' ERROR_RXACT_STATE_CREATED                                                = Just "ERROR_RXACT_STATE_CREATED"
displaySystemErrorCode' ERROR_SEGMENT_NOTIFICATION                                               = Just "ERROR_SEGMENT_NOTIFICATION"
displaySystemErrorCode' ERROR_BAD_CURRENT_DIRECTORY                                              = Just "ERROR_BAD_CURRENT_DIRECTORY"
displaySystemErrorCode' ERROR_FT_READ_RECOVERY_FROM_BACKUP                                       = Just "ERROR_FT_READ_RECOVERY_FROM_BACKUP"
displaySystemErrorCode' ERROR_FT_WRITE_RECOVERY                                                  = Just "ERROR_FT_WRITE_RECOVERY"
displaySystemErrorCode' ERROR_IMAGE_MACHINE_TYPE_MISMATCH                                        = Just "ERROR_IMAGE_MACHINE_TYPE_MISMATCH"
displaySystemErrorCode' ERROR_RECEIVE_PARTIAL                                                    = Just "ERROR_RECEIVE_PARTIAL"
displaySystemErrorCode' ERROR_RECEIVE_EXPEDITED                                                  = Just "ERROR_RECEIVE_EXPEDITED"
displaySystemErrorCode' ERROR_RECEIVE_PARTIAL_EXPEDITED                                          = Just "ERROR_RECEIVE_PARTIAL_EXPEDITED"
displaySystemErrorCode' ERROR_EVENT_DONE                                                         = Just "ERROR_EVENT_DONE"
displaySystemErrorCode' ERROR_EVENT_PENDING                                                      = Just "ERROR_EVENT_PENDING"
displaySystemErrorCode' ERROR_CHECKING_FILE_SYSTEM                                               = Just "ERROR_CHECKING_FILE_SYSTEM"
displaySystemErrorCode' ERROR_FATAL_APP_EXIT                                                     = Just "ERROR_FATAL_APP_EXIT"
displaySystemErrorCode' ERROR_PREDEFINED_HANDLE                                                  = Just "ERROR_PREDEFINED_HANDLE"
displaySystemErrorCode' ERROR_WAS_UNLOCKED                                                       = Just "ERROR_WAS_UNLOCKED"
displaySystemErrorCode' ERROR_SERVICE_NOTIFICATION                                               = Just "ERROR_SERVICE_NOTIFICATION"
displaySystemErrorCode' ERROR_WAS_LOCKED                                                         = Just "ERROR_WAS_LOCKED"
displaySystemErrorCode' ERROR_LOG_HARD_ERROR                                                     = Just "ERROR_LOG_HARD_ERROR"
displaySystemErrorCode' ERROR_ALREADY_WIN32                                                      = Just "ERROR_ALREADY_WIN32"
displaySystemErrorCode' ERROR_IMAGE_MACHINE_TYPE_MISMATCH_EXE                                    = Just "ERROR_IMAGE_MACHINE_TYPE_MISMATCH_EXE"
displaySystemErrorCode' ERROR_NO_YIELD_PERFORMED                                                 = Just "ERROR_NO_YIELD_PERFORMED"
displaySystemErrorCode' ERROR_TIMER_RESUME_IGNORED                                               = Just "ERROR_TIMER_RESUME_IGNORED"
displaySystemErrorCode' ERROR_ARBITRATION_UNHANDLED                                              = Just "ERROR_ARBITRATION_UNHANDLED"
displaySystemErrorCode' ERROR_CARDBUS_NOT_SUPPORTED                                              = Just "ERROR_CARDBUS_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_MP_PROCESSOR_MISMATCH                                              = Just "ERROR_MP_PROCESSOR_MISMATCH"
displaySystemErrorCode' ERROR_HIBERNATED                                                         = Just "ERROR_HIBERNATED"
displaySystemErrorCode' ERROR_RESUME_HIBERNATION                                                 = Just "ERROR_RESUME_HIBERNATION"
displaySystemErrorCode' ERROR_FIRMWARE_UPDATED                                                   = Just "ERROR_FIRMWARE_UPDATED"
displaySystemErrorCode' ERROR_DRIVERS_LEAKING_LOCKED_PAGES                                       = Just "ERROR_DRIVERS_LEAKING_LOCKED_PAGES"
displaySystemErrorCode' ERROR_WAKE_SYSTEM                                                        = Just "ERROR_WAKE_SYSTEM"
displaySystemErrorCode' ERROR_WAIT_1                                                             = Just "ERROR_WAIT_1"
displaySystemErrorCode' ERROR_WAIT_2                                                             = Just "ERROR_WAIT_2"
displaySystemErrorCode' ERROR_WAIT_3                                                             = Just "ERROR_WAIT_3"
displaySystemErrorCode' ERROR_WAIT_63                                                            = Just "ERROR_WAIT_63"
displaySystemErrorCode' ERROR_ABANDONED_WAIT_0                                                   = Just "ERROR_ABANDONED_WAIT_0"
displaySystemErrorCode' ERROR_ABANDONED_WAIT_63                                                  = Just "ERROR_ABANDONED_WAIT_63"
displaySystemErrorCode' ERROR_USER_APC                                                           = Just "ERROR_USER_APC"
displaySystemErrorCode' ERROR_KERNEL_APC                                                         = Just "ERROR_KERNEL_APC"
displaySystemErrorCode' ERROR_ALERTED                                                            = Just "ERROR_ALERTED"
displaySystemErrorCode' ERROR_ELEVATION_REQUIRED                                                 = Just "ERROR_ELEVATION_REQUIRED"
displaySystemErrorCode' ERROR_REPARSE                                                            = Just "ERROR_REPARSE"
displaySystemErrorCode' ERROR_OPLOCK_BREAK_IN_PROGRESS                                           = Just "ERROR_OPLOCK_BREAK_IN_PROGRESS"
displaySystemErrorCode' ERROR_VOLUME_MOUNTED                                                     = Just "ERROR_VOLUME_MOUNTED"
displaySystemErrorCode' ERROR_RXACT_COMMITTED                                                    = Just "ERROR_RXACT_COMMITTED"
displaySystemErrorCode' ERROR_NOTIFY_CLEANUP                                                     = Just "ERROR_NOTIFY_CLEANUP"
displaySystemErrorCode' ERROR_PRIMARY_TRANSPORT_CONNECT_FAILED                                   = Just "ERROR_PRIMARY_TRANSPORT_CONNECT_FAILED"
displaySystemErrorCode' ERROR_PAGE_FAULT_TRANSITION                                              = Just "ERROR_PAGE_FAULT_TRANSITION"
displaySystemErrorCode' ERROR_PAGE_FAULT_DEMAND_ZERO                                             = Just "ERROR_PAGE_FAULT_DEMAND_ZERO"
displaySystemErrorCode' ERROR_PAGE_FAULT_COPY_ON_WRITE                                           = Just "ERROR_PAGE_FAULT_COPY_ON_WRITE"
displaySystemErrorCode' ERROR_PAGE_FAULT_GUARD_PAGE                                              = Just "ERROR_PAGE_FAULT_GUARD_PAGE"
displaySystemErrorCode' ERROR_PAGE_FAULT_PAGING_FILE                                             = Just "ERROR_PAGE_FAULT_PAGING_FILE"
displaySystemErrorCode' ERROR_CACHE_PAGE_LOCKED                                                  = Just "ERROR_CACHE_PAGE_LOCKED"
displaySystemErrorCode' ERROR_CRASH_DUMP                                                         = Just "ERROR_CRASH_DUMP"
displaySystemErrorCode' ERROR_BUFFER_ALL_ZEROS                                                   = Just "ERROR_BUFFER_ALL_ZEROS"
displaySystemErrorCode' ERROR_REPARSE_OBJECT                                                     = Just "ERROR_REPARSE_OBJECT"
displaySystemErrorCode' ERROR_RESOURCE_REQUIREMENTS_CHANGED                                      = Just "ERROR_RESOURCE_REQUIREMENTS_CHANGED"
displaySystemErrorCode' ERROR_TRANSLATION_COMPLETE                                               = Just "ERROR_TRANSLATION_COMPLETE"
displaySystemErrorCode' ERROR_NOTHING_TO_TERMINATE                                               = Just "ERROR_NOTHING_TO_TERMINATE"
displaySystemErrorCode' ERROR_PROCESS_NOT_IN_JOB                                                 = Just "ERROR_PROCESS_NOT_IN_JOB"
displaySystemErrorCode' ERROR_PROCESS_IN_JOB                                                     = Just "ERROR_PROCESS_IN_JOB"
displaySystemErrorCode' ERROR_VOLSNAP_HIBERNATE_READY                                            = Just "ERROR_VOLSNAP_HIBERNATE_READY"
displaySystemErrorCode' ERROR_FSFILTER_OP_COMPLETED_SUCCESSFULLY                                 = Just "ERROR_FSFILTER_OP_COMPLETED_SUCCESSFULLY"
displaySystemErrorCode' ERROR_INTERRUPT_VECTOR_ALREADY_CONNECTED                                 = Just "ERROR_INTERRUPT_VECTOR_ALREADY_CONNECTED"
displaySystemErrorCode' ERROR_INTERRUPT_STILL_CONNECTED                                          = Just "ERROR_INTERRUPT_STILL_CONNECTED"
displaySystemErrorCode' ERROR_WAIT_FOR_OPLOCK                                                    = Just "ERROR_WAIT_FOR_OPLOCK"
displaySystemErrorCode' ERROR_DBG_EXCEPTION_HANDLED                                              = Just "ERROR_DBG_EXCEPTION_HANDLED"
displaySystemErrorCode' ERROR_DBG_CONTINUE                                                       = Just "ERROR_DBG_CONTINUE"
displaySystemErrorCode' ERROR_CALLBACK_POP_STACK                                                 = Just "ERROR_CALLBACK_POP_STACK"
displaySystemErrorCode' ERROR_COMPRESSION_DISABLED                                               = Just "ERROR_COMPRESSION_DISABLED"
displaySystemErrorCode' ERROR_CANTFETCHBACKWARDS                                                 = Just "ERROR_CANTFETCHBACKWARDS"
displaySystemErrorCode' ERROR_CANTSCROLLBACKWARDS                                                = Just "ERROR_CANTSCROLLBACKWARDS"
displaySystemErrorCode' ERROR_ROWSNOTRELEASED                                                    = Just "ERROR_ROWSNOTRELEASED"
displaySystemErrorCode' ERROR_BAD_ACCESSOR_FLAGS                                                 = Just "ERROR_BAD_ACCESSOR_FLAGS"
displaySystemErrorCode' ERROR_ERRORS_ENCOUNTERED                                                 = Just "ERROR_ERRORS_ENCOUNTERED"
displaySystemErrorCode' ERROR_NOT_CAPABLE                                                        = Just "ERROR_NOT_CAPABLE"
displaySystemErrorCode' ERROR_REQUEST_OUT_OF_SEQUENCE                                            = Just "ERROR_REQUEST_OUT_OF_SEQUENCE"
displaySystemErrorCode' ERROR_VERSION_PARSE_ERROR                                                = Just "ERROR_VERSION_PARSE_ERROR"
displaySystemErrorCode' ERROR_BADSTARTPOSITION                                                   = Just "ERROR_BADSTARTPOSITION"
displaySystemErrorCode' ERROR_MEMORY_HARDWARE                                                    = Just "ERROR_MEMORY_HARDWARE"
displaySystemErrorCode' ERROR_DISK_REPAIR_DISABLED                                               = Just "ERROR_DISK_REPAIR_DISABLED"
displaySystemErrorCode' ERROR_INSUFFICIENT_RESOURCE_FOR_SPECIFIED_SHARED_SECTION_SIZE            = Just "ERROR_INSUFFICIENT_RESOURCE_FOR_SPECIFIED_SHARED_SECTION_SIZE"
displaySystemErrorCode' ERROR_SYSTEM_POWERSTATE_TRANSITION                                       = Just "ERROR_SYSTEM_POWERSTATE_TRANSITION"
displaySystemErrorCode' ERROR_SYSTEM_POWERSTATE_COMPLEX_TRANSITION                               = Just "ERROR_SYSTEM_POWERSTATE_COMPLEX_TRANSITION"
displaySystemErrorCode' ERROR_MCA_EXCEPTION                                                      = Just "ERROR_MCA_EXCEPTION"
displaySystemErrorCode' ERROR_ACCESS_AUDIT_BY_POLICY                                             = Just "ERROR_ACCESS_AUDIT_BY_POLICY"
displaySystemErrorCode' ERROR_ACCESS_DISABLED_NO_SAFER_UI_BY_POLICY                              = Just "ERROR_ACCESS_DISABLED_NO_SAFER_UI_BY_POLICY"
displaySystemErrorCode' ERROR_ABANDON_HIBERFILE                                                  = Just "ERROR_ABANDON_HIBERFILE"
displaySystemErrorCode' ERROR_LOST_WRITEBEHIND_DATA_NETWORK_DISCONNECTED                         = Just "ERROR_LOST_WRITEBEHIND_DATA_NETWORK_DISCONNECTED"
displaySystemErrorCode' ERROR_LOST_WRITEBEHIND_DATA_NETWORK_SERVER_ERROR                         = Just "ERROR_LOST_WRITEBEHIND_DATA_NETWORK_SERVER_ERROR"
displaySystemErrorCode' ERROR_LOST_WRITEBEHIND_DATA_LOCAL_DISK_ERROR                             = Just "ERROR_LOST_WRITEBEHIND_DATA_LOCAL_DISK_ERROR"
displaySystemErrorCode' ERROR_BAD_MCFG_TABLE                                                     = Just "ERROR_BAD_MCFG_TABLE"
displaySystemErrorCode' ERROR_DISK_REPAIR_REDIRECTED                                             = Just "ERROR_DISK_REPAIR_REDIRECTED"
displaySystemErrorCode' ERROR_DISK_REPAIR_UNSUCCESSFUL                                           = Just "ERROR_DISK_REPAIR_UNSUCCESSFUL"
displaySystemErrorCode' ERROR_CORRUPT_LOG_OVERFULL                                               = Just "ERROR_CORRUPT_LOG_OVERFULL"
displaySystemErrorCode' ERROR_CORRUPT_LOG_CORRUPTED                                              = Just "ERROR_CORRUPT_LOG_CORRUPTED"
displaySystemErrorCode' ERROR_CORRUPT_LOG_UNAVAILABLE                                            = Just "ERROR_CORRUPT_LOG_UNAVAILABLE"
displaySystemErrorCode' ERROR_CORRUPT_LOG_DELETED_FULL                                           = Just "ERROR_CORRUPT_LOG_DELETED_FULL"
displaySystemErrorCode' ERROR_CORRUPT_LOG_CLEARED                                                = Just "ERROR_CORRUPT_LOG_CLEARED"
displaySystemErrorCode' ERROR_ORPHAN_NAME_EXHAUSTED                                              = Just "ERROR_ORPHAN_NAME_EXHAUSTED"
displaySystemErrorCode' ERROR_OPLOCK_SWITCHED_TO_NEW_HANDLE                                      = Just "ERROR_OPLOCK_SWITCHED_TO_NEW_HANDLE"
displaySystemErrorCode' ERROR_CANNOT_GRANT_REQUESTED_OPLOCK                                      = Just "ERROR_CANNOT_GRANT_REQUESTED_OPLOCK"
displaySystemErrorCode' ERROR_CANNOT_BREAK_OPLOCK                                                = Just "ERROR_CANNOT_BREAK_OPLOCK"
displaySystemErrorCode' ERROR_OPLOCK_HANDLE_CLOSED                                               = Just "ERROR_OPLOCK_HANDLE_CLOSED"
displaySystemErrorCode' ERROR_NO_ACE_CONDITION                                                   = Just "ERROR_NO_ACE_CONDITION"
displaySystemErrorCode' ERROR_INVALID_ACE_CONDITION                                              = Just "ERROR_INVALID_ACE_CONDITION"
displaySystemErrorCode' ERROR_FILE_HANDLE_REVOKED                                                = Just "ERROR_FILE_HANDLE_REVOKED"
displaySystemErrorCode' ERROR_IMAGE_AT_DIFFERENT_BASE                                            = Just "ERROR_IMAGE_AT_DIFFERENT_BASE"
displaySystemErrorCode' ERROR_ENCRYPTED_IO_NOT_POSSIBLE                                          = Just "ERROR_ENCRYPTED_IO_NOT_POSSIBLE"
displaySystemErrorCode' ERROR_EA_ACCESS_DENIED                                                   = Just "ERROR_EA_ACCESS_DENIED"
displaySystemErrorCode' ERROR_OPERATION_ABORTED                                                  = Just "ERROR_OPERATION_ABORTED"
displaySystemErrorCode' ERROR_IO_INCOMPLETE                                                      = Just "ERROR_IO_INCOMPLETE"
displaySystemErrorCode' ERROR_IO_PENDING                                                         = Just "ERROR_IO_PENDING"
displaySystemErrorCode' ERROR_NOACCESS                                                           = Just "ERROR_NOACCESS"
displaySystemErrorCode' ERROR_SWAPERROR                                                          = Just "ERROR_SWAPERROR"
displaySystemErrorCode' ERROR_STACK_OVERFLOW                                                     = Just "ERROR_STACK_OVERFLOW"
displaySystemErrorCode' ERROR_INVALID_MESSAGE                                                    = Just "ERROR_INVALID_MESSAGE"
displaySystemErrorCode' ERROR_CAN_NOT_COMPLETE                                                   = Just "ERROR_CAN_NOT_COMPLETE"
displaySystemErrorCode' ERROR_INVALID_FLAGS                                                      = Just "ERROR_INVALID_FLAGS"
displaySystemErrorCode' ERROR_UNRECOGNIZED_VOLUME                                                = Just "ERROR_UNRECOGNIZED_VOLUME"
displaySystemErrorCode' ERROR_FILE_INVALID                                                       = Just "ERROR_FILE_INVALID"
displaySystemErrorCode' ERROR_FULLSCREEN_MODE                                                    = Just "ERROR_FULLSCREEN_MODE"
displaySystemErrorCode' ERROR_NO_TOKEN                                                           = Just "ERROR_NO_TOKEN"
displaySystemErrorCode' ERROR_BADDB                                                              = Just "ERROR_BADDB"
displaySystemErrorCode' ERROR_BADKEY                                                             = Just "ERROR_BADKEY"
displaySystemErrorCode' ERROR_CANTOPEN                                                           = Just "ERROR_CANTOPEN"
displaySystemErrorCode' ERROR_CANTREAD                                                           = Just "ERROR_CANTREAD"
displaySystemErrorCode' ERROR_CANTWRITE                                                          = Just "ERROR_CANTWRITE"
displaySystemErrorCode' ERROR_REGISTRY_RECOVERED                                                 = Just "ERROR_REGISTRY_RECOVERED"
displaySystemErrorCode' ERROR_REGISTRY_CORRUPT                                                   = Just "ERROR_REGISTRY_CORRUPT"
displaySystemErrorCode' ERROR_REGISTRY_IO_FAILED                                                 = Just "ERROR_REGISTRY_IO_FAILED"
displaySystemErrorCode' ERROR_NOT_REGISTRY_FILE                                                  = Just "ERROR_NOT_REGISTRY_FILE"
displaySystemErrorCode' ERROR_KEY_DELETED                                                        = Just "ERROR_KEY_DELETED"
displaySystemErrorCode' ERROR_NO_LOG_SPACE                                                       = Just "ERROR_NO_LOG_SPACE"
displaySystemErrorCode' ERROR_KEY_HAS_CHILDREN                                                   = Just "ERROR_KEY_HAS_CHILDREN"
displaySystemErrorCode' ERROR_CHILD_MUST_BE_VOLATILE                                             = Just "ERROR_CHILD_MUST_BE_VOLATILE"
displaySystemErrorCode' ERROR_NOTIFY_ENUM_DIR                                                    = Just "ERROR_NOTIFY_ENUM_DIR"
displaySystemErrorCode' ERROR_DEPENDENT_SERVICES_RUNNING                                         = Just "ERROR_DEPENDENT_SERVICES_RUNNING"
displaySystemErrorCode' ERROR_INVALID_SERVICE_CONTROL                                            = Just "ERROR_INVALID_SERVICE_CONTROL"
displaySystemErrorCode' ERROR_SERVICE_REQUEST_TIMEOUT                                            = Just "ERROR_SERVICE_REQUEST_TIMEOUT"
displaySystemErrorCode' ERROR_SERVICE_NO_THREAD                                                  = Just "ERROR_SERVICE_NO_THREAD"
displaySystemErrorCode' ERROR_SERVICE_DATABASE_LOCKED                                            = Just "ERROR_SERVICE_DATABASE_LOCKED"
displaySystemErrorCode' ERROR_SERVICE_ALREADY_RUNNING                                            = Just "ERROR_SERVICE_ALREADY_RUNNING"
displaySystemErrorCode' ERROR_INVALID_SERVICE_ACCOUNT                                            = Just "ERROR_INVALID_SERVICE_ACCOUNT"
displaySystemErrorCode' ERROR_SERVICE_DISABLED                                                   = Just "ERROR_SERVICE_DISABLED"
displaySystemErrorCode' ERROR_CIRCULAR_DEPENDENCY                                                = Just "ERROR_CIRCULAR_DEPENDENCY"
displaySystemErrorCode' ERROR_SERVICE_DOES_NOT_EXIST                                             = Just "ERROR_SERVICE_DOES_NOT_EXIST"
displaySystemErrorCode' ERROR_SERVICE_CANNOT_ACCEPT_CTRL                                         = Just "ERROR_SERVICE_CANNOT_ACCEPT_CTRL"
displaySystemErrorCode' ERROR_SERVICE_NOT_ACTIVE                                                 = Just "ERROR_SERVICE_NOT_ACTIVE"
displaySystemErrorCode' ERROR_FAILED_SERVICE_CONTROLLER_CONNECT                                  = Just "ERROR_FAILED_SERVICE_CONTROLLER_CONNECT"
displaySystemErrorCode' ERROR_EXCEPTION_IN_SERVICE                                               = Just "ERROR_EXCEPTION_IN_SERVICE"
displaySystemErrorCode' ERROR_DATABASE_DOES_NOT_EXIST                                            = Just "ERROR_DATABASE_DOES_NOT_EXIST"
displaySystemErrorCode' ERROR_SERVICE_SPECIFIC_ERROR                                             = Just "ERROR_SERVICE_SPECIFIC_ERROR"
displaySystemErrorCode' ERROR_PROCESS_ABORTED                                                    = Just "ERROR_PROCESS_ABORTED"
displaySystemErrorCode' ERROR_SERVICE_DEPENDENCY_FAIL                                            = Just "ERROR_SERVICE_DEPENDENCY_FAIL"
displaySystemErrorCode' ERROR_SERVICE_LOGON_FAILED                                               = Just "ERROR_SERVICE_LOGON_FAILED"
displaySystemErrorCode' ERROR_SERVICE_START_HANG                                                 = Just "ERROR_SERVICE_START_HANG"
displaySystemErrorCode' ERROR_INVALID_SERVICE_LOCK                                               = Just "ERROR_INVALID_SERVICE_LOCK"
displaySystemErrorCode' ERROR_SERVICE_MARKED_FOR_DELETE                                          = Just "ERROR_SERVICE_MARKED_FOR_DELETE"
displaySystemErrorCode' ERROR_SERVICE_EXISTS                                                     = Just "ERROR_SERVICE_EXISTS"
displaySystemErrorCode' ERROR_ALREADY_RUNNING_LKG                                                = Just "ERROR_ALREADY_RUNNING_LKG"
displaySystemErrorCode' ERROR_SERVICE_DEPENDENCY_DELETED                                         = Just "ERROR_SERVICE_DEPENDENCY_DELETED"
displaySystemErrorCode' ERROR_BOOT_ALREADY_ACCEPTED                                              = Just "ERROR_BOOT_ALREADY_ACCEPTED"
displaySystemErrorCode' ERROR_SERVICE_NEVER_STARTED                                              = Just "ERROR_SERVICE_NEVER_STARTED"
displaySystemErrorCode' ERROR_DUPLICATE_SERVICE_NAME                                             = Just "ERROR_DUPLICATE_SERVICE_NAME"
displaySystemErrorCode' ERROR_DIFFERENT_SERVICE_ACCOUNT                                          = Just "ERROR_DIFFERENT_SERVICE_ACCOUNT"
displaySystemErrorCode' ERROR_CANNOT_DETECT_DRIVER_FAILURE                                       = Just "ERROR_CANNOT_DETECT_DRIVER_FAILURE"
displaySystemErrorCode' ERROR_CANNOT_DETECT_PROCESS_ABORT                                        = Just "ERROR_CANNOT_DETECT_PROCESS_ABORT"
displaySystemErrorCode' ERROR_NO_RECOVERY_PROGRAM                                                = Just "ERROR_NO_RECOVERY_PROGRAM"
displaySystemErrorCode' ERROR_SERVICE_NOT_IN_EXE                                                 = Just "ERROR_SERVICE_NOT_IN_EXE"
displaySystemErrorCode' ERROR_NOT_SAFEBOOT_SERVICE                                               = Just "ERROR_NOT_SAFEBOOT_SERVICE"
displaySystemErrorCode' ERROR_END_OF_MEDIA                                                       = Just "ERROR_END_OF_MEDIA"
displaySystemErrorCode' ERROR_FILEMARK_DETECTED                                                  = Just "ERROR_FILEMARK_DETECTED"
displaySystemErrorCode' ERROR_BEGINNING_OF_MEDIA                                                 = Just "ERROR_BEGINNING_OF_MEDIA"
displaySystemErrorCode' ERROR_SETMARK_DETECTED                                                   = Just "ERROR_SETMARK_DETECTED"
displaySystemErrorCode' ERROR_NO_DATA_DETECTED                                                   = Just "ERROR_NO_DATA_DETECTED"
displaySystemErrorCode' ERROR_PARTITION_FAILURE                                                  = Just "ERROR_PARTITION_FAILURE"
displaySystemErrorCode' ERROR_INVALID_BLOCK_LENGTH                                               = Just "ERROR_INVALID_BLOCK_LENGTH"
displaySystemErrorCode' ERROR_DEVICE_NOT_PARTITIONED                                             = Just "ERROR_DEVICE_NOT_PARTITIONED"
displaySystemErrorCode' ERROR_UNABLE_TO_LOCK_MEDIA                                               = Just "ERROR_UNABLE_TO_LOCK_MEDIA"
displaySystemErrorCode' ERROR_UNABLE_TO_UNLOAD_MEDIA                                             = Just "ERROR_UNABLE_TO_UNLOAD_MEDIA"
displaySystemErrorCode' ERROR_MEDIA_CHANGED                                                      = Just "ERROR_MEDIA_CHANGED"
displaySystemErrorCode' ERROR_BUS_RESET                                                          = Just "ERROR_BUS_RESET"
displaySystemErrorCode' ERROR_NO_MEDIA_IN_DRIVE                                                  = Just "ERROR_NO_MEDIA_IN_DRIVE"
displaySystemErrorCode' ERROR_NO_UNICODE_TRANSLATION                                             = Just "ERROR_NO_UNICODE_TRANSLATION"
displaySystemErrorCode' ERROR_DLL_INIT_FAILED                                                    = Just "ERROR_DLL_INIT_FAILED"
displaySystemErrorCode' ERROR_SHUTDOWN_IN_PROGRESS                                               = Just "ERROR_SHUTDOWN_IN_PROGRESS"
displaySystemErrorCode' ERROR_NO_SHUTDOWN_IN_PROGRESS                                            = Just "ERROR_NO_SHUTDOWN_IN_PROGRESS"
displaySystemErrorCode' ERROR_IO_DEVICE                                                          = Just "ERROR_IO_DEVICE"
displaySystemErrorCode' ERROR_SERIAL_NO_DEVICE                                                   = Just "ERROR_SERIAL_NO_DEVICE"
displaySystemErrorCode' ERROR_IRQ_BUSY                                                           = Just "ERROR_IRQ_BUSY"
displaySystemErrorCode' ERROR_MORE_WRITES                                                        = Just "ERROR_MORE_WRITES"
displaySystemErrorCode' ERROR_COUNTER_TIMEOUT                                                    = Just "ERROR_COUNTER_TIMEOUT"
displaySystemErrorCode' ERROR_FLOPPY_ID_MARK_NOT_FOUND                                           = Just "ERROR_FLOPPY_ID_MARK_NOT_FOUND"
displaySystemErrorCode' ERROR_FLOPPY_WRONG_CYLINDER                                              = Just "ERROR_FLOPPY_WRONG_CYLINDER"
displaySystemErrorCode' ERROR_FLOPPY_UNKNOWN_ERROR                                               = Just "ERROR_FLOPPY_UNKNOWN_ERROR"
displaySystemErrorCode' ERROR_FLOPPY_BAD_REGISTERS                                               = Just "ERROR_FLOPPY_BAD_REGISTERS"
displaySystemErrorCode' ERROR_DISK_RECALIBRATE_FAILED                                            = Just "ERROR_DISK_RECALIBRATE_FAILED"
displaySystemErrorCode' ERROR_DISK_OPERATION_FAILED                                              = Just "ERROR_DISK_OPERATION_FAILED"
displaySystemErrorCode' ERROR_DISK_RESET_FAILED                                                  = Just "ERROR_DISK_RESET_FAILED"
displaySystemErrorCode' ERROR_EOM_OVERFLOW                                                       = Just "ERROR_EOM_OVERFLOW"
displaySystemErrorCode' ERROR_NOT_ENOUGH_SERVER_MEMORY                                           = Just "ERROR_NOT_ENOUGH_SERVER_MEMORY"
displaySystemErrorCode' ERROR_POSSIBLE_DEADLOCK                                                  = Just "ERROR_POSSIBLE_DEADLOCK"
displaySystemErrorCode' ERROR_MAPPED_ALIGNMENT                                                   = Just "ERROR_MAPPED_ALIGNMENT"
displaySystemErrorCode' ERROR_SET_POWER_STATE_VETOED                                             = Just "ERROR_SET_POWER_STATE_VETOED"
displaySystemErrorCode' ERROR_SET_POWER_STATE_FAILED                                             = Just "ERROR_SET_POWER_STATE_FAILED"
displaySystemErrorCode' ERROR_TOO_MANY_LINKS                                                     = Just "ERROR_TOO_MANY_LINKS"
displaySystemErrorCode' ERROR_OLD_WIN_VERSION                                                    = Just "ERROR_OLD_WIN_VERSION"
displaySystemErrorCode' ERROR_APP_WRONG_OS                                                       = Just "ERROR_APP_WRONG_OS"
displaySystemErrorCode' ERROR_SINGLE_INSTANCE_APP                                                = Just "ERROR_SINGLE_INSTANCE_APP"
displaySystemErrorCode' ERROR_RMODE_APP                                                          = Just "ERROR_RMODE_APP"
displaySystemErrorCode' ERROR_INVALID_DLL                                                        = Just "ERROR_INVALID_DLL"
displaySystemErrorCode' ERROR_NO_ASSOCIATION                                                     = Just "ERROR_NO_ASSOCIATION"
displaySystemErrorCode' ERROR_DDE_FAIL                                                           = Just "ERROR_DDE_FAIL"
displaySystemErrorCode' ERROR_DLL_NOT_FOUND                                                      = Just "ERROR_DLL_NOT_FOUND"
displaySystemErrorCode' ERROR_NO_MORE_USER_HANDLES                                               = Just "ERROR_NO_MORE_USER_HANDLES"
displaySystemErrorCode' ERROR_MESSAGE_SYNC_ONLY                                                  = Just "ERROR_MESSAGE_SYNC_ONLY"
displaySystemErrorCode' ERROR_SOURCE_ELEMENT_EMPTY                                               = Just "ERROR_SOURCE_ELEMENT_EMPTY"
displaySystemErrorCode' ERROR_DESTINATION_ELEMENT_FULL                                           = Just "ERROR_DESTINATION_ELEMENT_FULL"
displaySystemErrorCode' ERROR_ILLEGAL_ELEMENT_ADDRESS                                            = Just "ERROR_ILLEGAL_ELEMENT_ADDRESS"
displaySystemErrorCode' ERROR_MAGAZINE_NOT_PRESENT                                               = Just "ERROR_MAGAZINE_NOT_PRESENT"
displaySystemErrorCode' ERROR_DEVICE_REINITIALIZATION_NEEDED                                     = Just "ERROR_DEVICE_REINITIALIZATION_NEEDED"
displaySystemErrorCode' ERROR_DEVICE_REQUIRES_CLEANING                                           = Just "ERROR_DEVICE_REQUIRES_CLEANING"
displaySystemErrorCode' ERROR_DEVICE_DOOR_OPEN                                                   = Just "ERROR_DEVICE_DOOR_OPEN"
displaySystemErrorCode' ERROR_DEVICE_NOT_CONNECTED                                               = Just "ERROR_DEVICE_NOT_CONNECTED"
displaySystemErrorCode' ERROR_NOT_FOUND                                                          = Just "ERROR_NOT_FOUND"
displaySystemErrorCode' ERROR_NO_MATCH                                                           = Just "ERROR_NO_MATCH"
displaySystemErrorCode' ERROR_SET_NOT_FOUND                                                      = Just "ERROR_SET_NOT_FOUND"
displaySystemErrorCode' ERROR_POINT_NOT_FOUND                                                    = Just "ERROR_POINT_NOT_FOUND"
displaySystemErrorCode' ERROR_NO_TRACKING_SERVICE                                                = Just "ERROR_NO_TRACKING_SERVICE"
displaySystemErrorCode' ERROR_NO_VOLUME_ID                                                       = Just "ERROR_NO_VOLUME_ID"
displaySystemErrorCode' ERROR_UNABLE_TO_REMOVE_REPLACED                                          = Just "ERROR_UNABLE_TO_REMOVE_REPLACED"
displaySystemErrorCode' ERROR_UNABLE_TO_MOVE_REPLACEMENT                                         = Just "ERROR_UNABLE_TO_MOVE_REPLACEMENT"
displaySystemErrorCode' ERROR_UNABLE_TO_MOVE_REPLACEMENT_2                                       = Just "ERROR_UNABLE_TO_MOVE_REPLACEMENT_2"
displaySystemErrorCode' ERROR_JOURNAL_DELETE_IN_PROGRESS                                         = Just "ERROR_JOURNAL_DELETE_IN_PROGRESS"
displaySystemErrorCode' ERROR_JOURNAL_NOT_ACTIVE                                                 = Just "ERROR_JOURNAL_NOT_ACTIVE"
displaySystemErrorCode' ERROR_POTENTIAL_FILE_FOUND                                               = Just "ERROR_POTENTIAL_FILE_FOUND"
displaySystemErrorCode' ERROR_JOURNAL_ENTRY_DELETED                                              = Just "ERROR_JOURNAL_ENTRY_DELETED"
displaySystemErrorCode' ERROR_SHUTDOWN_IS_SCHEDULED                                              = Just "ERROR_SHUTDOWN_IS_SCHEDULED"
displaySystemErrorCode' ERROR_SHUTDOWN_USERS_LOGGED_ON                                           = Just "ERROR_SHUTDOWN_USERS_LOGGED_ON"
displaySystemErrorCode' ERROR_BAD_DEVICE                                                         = Just "ERROR_BAD_DEVICE"
displaySystemErrorCode' ERROR_CONNECTION_UNAVAIL                                                 = Just "ERROR_CONNECTION_UNAVAIL"
displaySystemErrorCode' ERROR_DEVICE_ALREADY_REMEMBERED                                          = Just "ERROR_DEVICE_ALREADY_REMEMBERED"
displaySystemErrorCode' ERROR_NO_NET_OR_BAD_PATH                                                 = Just "ERROR_NO_NET_OR_BAD_PATH"
displaySystemErrorCode' ERROR_BAD_PROVIDER                                                       = Just "ERROR_BAD_PROVIDER"
displaySystemErrorCode' ERROR_CANNOT_OPEN_PROFILE                                                = Just "ERROR_CANNOT_OPEN_PROFILE"
displaySystemErrorCode' ERROR_BAD_PROFILE                                                        = Just "ERROR_BAD_PROFILE"
displaySystemErrorCode' ERROR_NOT_CONTAINER                                                      = Just "ERROR_NOT_CONTAINER"
displaySystemErrorCode' ERROR_EXTENDED_ERROR                                                     = Just "ERROR_EXTENDED_ERROR"
displaySystemErrorCode' ERROR_INVALID_GROUPNAME                                                  = Just "ERROR_INVALID_GROUPNAME"
displaySystemErrorCode' ERROR_INVALID_COMPUTERNAME                                               = Just "ERROR_INVALID_COMPUTERNAME"
displaySystemErrorCode' ERROR_INVALID_EVENTNAME                                                  = Just "ERROR_INVALID_EVENTNAME"
displaySystemErrorCode' ERROR_INVALID_DOMAINNAME                                                 = Just "ERROR_INVALID_DOMAINNAME"
displaySystemErrorCode' ERROR_INVALID_SERVICENAME                                                = Just "ERROR_INVALID_SERVICENAME"
displaySystemErrorCode' ERROR_INVALID_NETNAME                                                    = Just "ERROR_INVALID_NETNAME"
displaySystemErrorCode' ERROR_INVALID_SHARENAME                                                  = Just "ERROR_INVALID_SHARENAME"
displaySystemErrorCode' ERROR_INVALID_PASSWORDNAME                                               = Just "ERROR_INVALID_PASSWORDNAME"
displaySystemErrorCode' ERROR_INVALID_MESSAGENAME                                                = Just "ERROR_INVALID_MESSAGENAME"
displaySystemErrorCode' ERROR_INVALID_MESSAGEDEST                                                = Just "ERROR_INVALID_MESSAGEDEST"
displaySystemErrorCode' ERROR_SESSION_CREDENTIAL_CONFLICT                                        = Just "ERROR_SESSION_CREDENTIAL_CONFLICT"
displaySystemErrorCode' ERROR_REMOTE_SESSION_LIMIT_EXCEEDED                                      = Just "ERROR_REMOTE_SESSION_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_DUP_DOMAINNAME                                                     = Just "ERROR_DUP_DOMAINNAME"
displaySystemErrorCode' ERROR_NO_NETWORK                                                         = Just "ERROR_NO_NETWORK"
displaySystemErrorCode' ERROR_CANCELLED                                                          = Just "ERROR_CANCELLED"
displaySystemErrorCode' ERROR_USER_MAPPED_FILE                                                   = Just "ERROR_USER_MAPPED_FILE"
displaySystemErrorCode' ERROR_CONNECTION_REFUSED                                                 = Just "ERROR_CONNECTION_REFUSED"
displaySystemErrorCode' ERROR_GRACEFUL_DISCONNECT                                                = Just "ERROR_GRACEFUL_DISCONNECT"
displaySystemErrorCode' ERROR_ADDRESS_ALREADY_ASSOCIATED                                         = Just "ERROR_ADDRESS_ALREADY_ASSOCIATED"
displaySystemErrorCode' ERROR_ADDRESS_NOT_ASSOCIATED                                             = Just "ERROR_ADDRESS_NOT_ASSOCIATED"
displaySystemErrorCode' ERROR_CONNECTION_INVALID                                                 = Just "ERROR_CONNECTION_INVALID"
displaySystemErrorCode' ERROR_CONNECTION_ACTIVE                                                  = Just "ERROR_CONNECTION_ACTIVE"
displaySystemErrorCode' ERROR_NETWORK_UNREACHABLE                                                = Just "ERROR_NETWORK_UNREACHABLE"
displaySystemErrorCode' ERROR_HOST_UNREACHABLE                                                   = Just "ERROR_HOST_UNREACHABLE"
displaySystemErrorCode' ERROR_PROTOCOL_UNREACHABLE                                               = Just "ERROR_PROTOCOL_UNREACHABLE"
displaySystemErrorCode' ERROR_PORT_UNREACHABLE                                                   = Just "ERROR_PORT_UNREACHABLE"
displaySystemErrorCode' ERROR_REQUEST_ABORTED                                                    = Just "ERROR_REQUEST_ABORTED"
displaySystemErrorCode' ERROR_CONNECTION_ABORTED                                                 = Just "ERROR_CONNECTION_ABORTED"
displaySystemErrorCode' ERROR_RETRY                                                              = Just "ERROR_RETRY"
displaySystemErrorCode' ERROR_CONNECTION_COUNT_LIMIT                                             = Just "ERROR_CONNECTION_COUNT_LIMIT"
displaySystemErrorCode' ERROR_LOGIN_TIME_RESTRICTION                                             = Just "ERROR_LOGIN_TIME_RESTRICTION"
displaySystemErrorCode' ERROR_LOGIN_WKSTA_RESTRICTION                                            = Just "ERROR_LOGIN_WKSTA_RESTRICTION"
displaySystemErrorCode' ERROR_INCORRECT_ADDRESS                                                  = Just "ERROR_INCORRECT_ADDRESS"
displaySystemErrorCode' ERROR_ALREADY_REGISTERED                                                 = Just "ERROR_ALREADY_REGISTERED"
displaySystemErrorCode' ERROR_SERVICE_NOT_FOUND                                                  = Just "ERROR_SERVICE_NOT_FOUND"
displaySystemErrorCode' ERROR_NOT_AUTHENTICATED                                                  = Just "ERROR_NOT_AUTHENTICATED"
displaySystemErrorCode' ERROR_NOT_LOGGED_ON                                                      = Just "ERROR_NOT_LOGGED_ON"
displaySystemErrorCode' ERROR_CONTINUE                                                           = Just "ERROR_CONTINUE"
displaySystemErrorCode' ERROR_ALREADY_INITIALIZED                                                = Just "ERROR_ALREADY_INITIALIZED"
displaySystemErrorCode' ERROR_NO_MORE_DEVICES                                                    = Just "ERROR_NO_MORE_DEVICES"
displaySystemErrorCode' ERROR_NO_SUCH_SITE                                                       = Just "ERROR_NO_SUCH_SITE"
displaySystemErrorCode' ERROR_DOMAIN_CONTROLLER_EXISTS                                           = Just "ERROR_DOMAIN_CONTROLLER_EXISTS"
displaySystemErrorCode' ERROR_ONLY_IF_CONNECTED                                                  = Just "ERROR_ONLY_IF_CONNECTED"
displaySystemErrorCode' ERROR_OVERRIDE_NOCHANGES                                                 = Just "ERROR_OVERRIDE_NOCHANGES"
displaySystemErrorCode' ERROR_BAD_USER_PROFILE                                                   = Just "ERROR_BAD_USER_PROFILE"
displaySystemErrorCode' ERROR_NOT_SUPPORTED_ON_SBS                                               = Just "ERROR_NOT_SUPPORTED_ON_SBS"
displaySystemErrorCode' ERROR_SERVER_SHUTDOWN_IN_PROGRESS                                        = Just "ERROR_SERVER_SHUTDOWN_IN_PROGRESS"
displaySystemErrorCode' ERROR_HOST_DOWN                                                          = Just "ERROR_HOST_DOWN"
displaySystemErrorCode' ERROR_NON_ACCOUNT_SID                                                    = Just "ERROR_NON_ACCOUNT_SID"
displaySystemErrorCode' ERROR_NON_DOMAIN_SID                                                     = Just "ERROR_NON_DOMAIN_SID"
displaySystemErrorCode' ERROR_APPHELP_BLOCK                                                      = Just "ERROR_APPHELP_BLOCK"
displaySystemErrorCode' ERROR_ACCESS_DISABLED_BY_POLICY                                          = Just "ERROR_ACCESS_DISABLED_BY_POLICY"
displaySystemErrorCode' ERROR_REG_NAT_CONSUMPTION                                                = Just "ERROR_REG_NAT_CONSUMPTION"
displaySystemErrorCode' ERROR_CSCSHARE_OFFLINE                                                   = Just "ERROR_CSCSHARE_OFFLINE"
displaySystemErrorCode' ERROR_PKINIT_FAILURE                                                     = Just "ERROR_PKINIT_FAILURE"
displaySystemErrorCode' ERROR_SMARTCARD_SUBSYSTEM_FAILURE                                        = Just "ERROR_SMARTCARD_SUBSYSTEM_FAILURE"
displaySystemErrorCode' ERROR_DOWNGRADE_DETECTED                                                 = Just "ERROR_DOWNGRADE_DETECTED"
displaySystemErrorCode' ERROR_MACHINE_LOCKED                                                     = Just "ERROR_MACHINE_LOCKED"
displaySystemErrorCode' ERROR_CALLBACK_SUPPLIED_INVALID_DATA                                     = Just "ERROR_CALLBACK_SUPPLIED_INVALID_DATA"
displaySystemErrorCode' ERROR_SYNC_FOREGROUND_REFRESH_REQUIRED                                   = Just "ERROR_SYNC_FOREGROUND_REFRESH_REQUIRED"
displaySystemErrorCode' ERROR_DRIVER_BLOCKED                                                     = Just "ERROR_DRIVER_BLOCKED"
displaySystemErrorCode' ERROR_INVALID_IMPORT_OF_NON_DLL                                          = Just "ERROR_INVALID_IMPORT_OF_NON_DLL"
displaySystemErrorCode' ERROR_ACCESS_DISABLED_WEBBLADE                                           = Just "ERROR_ACCESS_DISABLED_WEBBLADE"
displaySystemErrorCode' ERROR_ACCESS_DISABLED_WEBBLADE_TAMPER                                    = Just "ERROR_ACCESS_DISABLED_WEBBLADE_TAMPER"
displaySystemErrorCode' ERROR_RECOVERY_FAILURE                                                   = Just "ERROR_RECOVERY_FAILURE"
displaySystemErrorCode' ERROR_ALREADY_FIBER                                                      = Just "ERROR_ALREADY_FIBER"
displaySystemErrorCode' ERROR_ALREADY_THREAD                                                     = Just "ERROR_ALREADY_THREAD"
displaySystemErrorCode' ERROR_STACK_BUFFER_OVERRUN                                               = Just "ERROR_STACK_BUFFER_OVERRUN"
displaySystemErrorCode' ERROR_PARAMETER_QUOTA_EXCEEDED                                           = Just "ERROR_PARAMETER_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_DEBUGGER_INACTIVE                                                  = Just "ERROR_DEBUGGER_INACTIVE"
displaySystemErrorCode' ERROR_DELAY_LOAD_FAILED                                                  = Just "ERROR_DELAY_LOAD_FAILED"
displaySystemErrorCode' ERROR_VDM_DISALLOWED                                                     = Just "ERROR_VDM_DISALLOWED"
displaySystemErrorCode' ERROR_UNIDENTIFIED_ERROR                                                 = Just "ERROR_UNIDENTIFIED_ERROR"
displaySystemErrorCode' ERROR_INVALID_CRUNTIME_PARAMETER                                         = Just "ERROR_INVALID_CRUNTIME_PARAMETER"
displaySystemErrorCode' ERROR_BEYOND_VDL                                                         = Just "ERROR_BEYOND_VDL"
displaySystemErrorCode' ERROR_INCOMPATIBLE_SERVICE_SID_TYPE                                      = Just "ERROR_INCOMPATIBLE_SERVICE_SID_TYPE"
displaySystemErrorCode' ERROR_DRIVER_PROCESS_TERMINATED                                          = Just "ERROR_DRIVER_PROCESS_TERMINATED"
displaySystemErrorCode' ERROR_IMPLEMENTATION_LIMIT                                               = Just "ERROR_IMPLEMENTATION_LIMIT"
displaySystemErrorCode' ERROR_PROCESS_IS_PROTECTED                                               = Just "ERROR_PROCESS_IS_PROTECTED"
displaySystemErrorCode' ERROR_SERVICE_NOTIFY_CLIENT_LAGGING                                      = Just "ERROR_SERVICE_NOTIFY_CLIENT_LAGGING"
displaySystemErrorCode' ERROR_DISK_QUOTA_EXCEEDED                                                = Just "ERROR_DISK_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_CONTENT_BLOCKED                                                    = Just "ERROR_CONTENT_BLOCKED"
displaySystemErrorCode' ERROR_INCOMPATIBLE_SERVICE_PRIVILEGE                                     = Just "ERROR_INCOMPATIBLE_SERVICE_PRIVILEGE"
displaySystemErrorCode' ERROR_APP_HANG                                                           = Just "ERROR_APP_HANG"
displaySystemErrorCode' ERROR_INVALID_LABEL                                                      = Just "ERROR_INVALID_LABEL"
displaySystemErrorCode' ERROR_NOT_ALL_ASSIGNED                                                   = Just "ERROR_NOT_ALL_ASSIGNED"
displaySystemErrorCode' ERROR_SOME_NOT_MAPPED                                                    = Just "ERROR_SOME_NOT_MAPPED"
displaySystemErrorCode' ERROR_NO_QUOTAS_FOR_ACCOUNT                                              = Just "ERROR_NO_QUOTAS_FOR_ACCOUNT"
displaySystemErrorCode' ERROR_LOCAL_USER_SESSION_KEY                                             = Just "ERROR_LOCAL_USER_SESSION_KEY"
displaySystemErrorCode' ERROR_NULL_LM_PASSWORD                                                   = Just "ERROR_NULL_LM_PASSWORD"
displaySystemErrorCode' ERROR_UNKNOWN_REVISION                                                   = Just "ERROR_UNKNOWN_REVISION"
displaySystemErrorCode' ERROR_REVISION_MISMATCH                                                  = Just "ERROR_REVISION_MISMATCH"
displaySystemErrorCode' ERROR_INVALID_OWNER                                                      = Just "ERROR_INVALID_OWNER"
displaySystemErrorCode' ERROR_INVALID_PRIMARY_GROUP                                              = Just "ERROR_INVALID_PRIMARY_GROUP"
displaySystemErrorCode' ERROR_NO_IMPERSONATION_TOKEN                                             = Just "ERROR_NO_IMPERSONATION_TOKEN"
displaySystemErrorCode' ERROR_CANT_DISABLE_MANDATORY                                             = Just "ERROR_CANT_DISABLE_MANDATORY"
displaySystemErrorCode' ERROR_NO_LOGON_SERVERS                                                   = Just "ERROR_NO_LOGON_SERVERS"
displaySystemErrorCode' ERROR_NO_SUCH_LOGON_SESSION                                              = Just "ERROR_NO_SUCH_LOGON_SESSION"
displaySystemErrorCode' ERROR_NO_SUCH_PRIVILEGE                                                  = Just "ERROR_NO_SUCH_PRIVILEGE"
displaySystemErrorCode' ERROR_PRIVILEGE_NOT_HELD                                                 = Just "ERROR_PRIVILEGE_NOT_HELD"
displaySystemErrorCode' ERROR_INVALID_ACCOUNT_NAME                                               = Just "ERROR_INVALID_ACCOUNT_NAME"
displaySystemErrorCode' ERROR_USER_EXISTS                                                        = Just "ERROR_USER_EXISTS"
displaySystemErrorCode' ERROR_NO_SUCH_USER                                                       = Just "ERROR_NO_SUCH_USER"
displaySystemErrorCode' ERROR_GROUP_EXISTS                                                       = Just "ERROR_GROUP_EXISTS"
displaySystemErrorCode' ERROR_NO_SUCH_GROUP                                                      = Just "ERROR_NO_SUCH_GROUP"
displaySystemErrorCode' ERROR_MEMBER_IN_GROUP                                                    = Just "ERROR_MEMBER_IN_GROUP"
displaySystemErrorCode' ERROR_MEMBER_NOT_IN_GROUP                                                = Just "ERROR_MEMBER_NOT_IN_GROUP"
displaySystemErrorCode' ERROR_LAST_ADMIN                                                         = Just "ERROR_LAST_ADMIN"
displaySystemErrorCode' ERROR_WRONG_PASSWORD                                                     = Just "ERROR_WRONG_PASSWORD"
displaySystemErrorCode' ERROR_ILL_FORMED_PASSWORD                                                = Just "ERROR_ILL_FORMED_PASSWORD"
displaySystemErrorCode' ERROR_PASSWORD_RESTRICTION                                               = Just "ERROR_PASSWORD_RESTRICTION"
displaySystemErrorCode' ERROR_LOGON_FAILURE                                                      = Just "ERROR_LOGON_FAILURE"
displaySystemErrorCode' ERROR_ACCOUNT_RESTRICTION                                                = Just "ERROR_ACCOUNT_RESTRICTION"
displaySystemErrorCode' ERROR_INVALID_LOGON_HOURS                                                = Just "ERROR_INVALID_LOGON_HOURS"
displaySystemErrorCode' ERROR_INVALID_WORKSTATION                                                = Just "ERROR_INVALID_WORKSTATION"
displaySystemErrorCode' ERROR_PASSWORD_EXPIRED                                                   = Just "ERROR_PASSWORD_EXPIRED"
displaySystemErrorCode' ERROR_ACCOUNT_DISABLED                                                   = Just "ERROR_ACCOUNT_DISABLED"
displaySystemErrorCode' ERROR_NONE_MAPPED                                                        = Just "ERROR_NONE_MAPPED"
displaySystemErrorCode' ERROR_TOO_MANY_LUIDS_REQUESTED                                           = Just "ERROR_TOO_MANY_LUIDS_REQUESTED"
displaySystemErrorCode' ERROR_LUIDS_EXHAUSTED                                                    = Just "ERROR_LUIDS_EXHAUSTED"
displaySystemErrorCode' ERROR_INVALID_SUB_AUTHORITY                                              = Just "ERROR_INVALID_SUB_AUTHORITY"
displaySystemErrorCode' ERROR_INVALID_ACL                                                        = Just "ERROR_INVALID_ACL"
displaySystemErrorCode' ERROR_INVALID_SID                                                        = Just "ERROR_INVALID_SID"
displaySystemErrorCode' ERROR_INVALID_SECURITY_DESCR                                             = Just "ERROR_INVALID_SECURITY_DESCR"
displaySystemErrorCode' ERROR_BAD_INHERITANCE_ACL                                                = Just "ERROR_BAD_INHERITANCE_ACL"
displaySystemErrorCode' ERROR_SERVER_DISABLED                                                    = Just "ERROR_SERVER_DISABLED"
displaySystemErrorCode' ERROR_SERVER_NOT_DISABLED                                                = Just "ERROR_SERVER_NOT_DISABLED"
displaySystemErrorCode' ERROR_INVALID_ID_AUTHORITY                                               = Just "ERROR_INVALID_ID_AUTHORITY"
displaySystemErrorCode' ERROR_ALLOTTED_SPACE_EXCEEDED                                            = Just "ERROR_ALLOTTED_SPACE_EXCEEDED"
displaySystemErrorCode' ERROR_INVALID_GROUP_ATTRIBUTES                                           = Just "ERROR_INVALID_GROUP_ATTRIBUTES"
displaySystemErrorCode' ERROR_BAD_IMPERSONATION_LEVEL                                            = Just "ERROR_BAD_IMPERSONATION_LEVEL"
displaySystemErrorCode' ERROR_CANT_OPEN_ANONYMOUS                                                = Just "ERROR_CANT_OPEN_ANONYMOUS"
displaySystemErrorCode' ERROR_BAD_VALIDATION_CLASS                                               = Just "ERROR_BAD_VALIDATION_CLASS"
displaySystemErrorCode' ERROR_BAD_TOKEN_TYPE                                                     = Just "ERROR_BAD_TOKEN_TYPE"
displaySystemErrorCode' ERROR_NO_SECURITY_ON_OBJECT                                              = Just "ERROR_NO_SECURITY_ON_OBJECT"
displaySystemErrorCode' ERROR_CANT_ACCESS_DOMAIN_INFO                                            = Just "ERROR_CANT_ACCESS_DOMAIN_INFO"
displaySystemErrorCode' ERROR_INVALID_SERVER_STATE                                               = Just "ERROR_INVALID_SERVER_STATE"
displaySystemErrorCode' ERROR_INVALID_DOMAIN_STATE                                               = Just "ERROR_INVALID_DOMAIN_STATE"
displaySystemErrorCode' ERROR_INVALID_DOMAIN_ROLE                                                = Just "ERROR_INVALID_DOMAIN_ROLE"
displaySystemErrorCode' ERROR_NO_SUCH_DOMAIN                                                     = Just "ERROR_NO_SUCH_DOMAIN"
displaySystemErrorCode' ERROR_DOMAIN_EXISTS                                                      = Just "ERROR_DOMAIN_EXISTS"
displaySystemErrorCode' ERROR_DOMAIN_LIMIT_EXCEEDED                                              = Just "ERROR_DOMAIN_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_INTERNAL_DB_CORRUPTION                                             = Just "ERROR_INTERNAL_DB_CORRUPTION"
displaySystemErrorCode' ERROR_INTERNAL_ERROR                                                     = Just "ERROR_INTERNAL_ERROR"
displaySystemErrorCode' ERROR_GENERIC_NOT_MAPPED                                                 = Just "ERROR_GENERIC_NOT_MAPPED"
displaySystemErrorCode' ERROR_BAD_DESCRIPTOR_FORMAT                                              = Just "ERROR_BAD_DESCRIPTOR_FORMAT"
displaySystemErrorCode' ERROR_NOT_LOGON_PROCESS                                                  = Just "ERROR_NOT_LOGON_PROCESS"
displaySystemErrorCode' ERROR_LOGON_SESSION_EXISTS                                               = Just "ERROR_LOGON_SESSION_EXISTS"
displaySystemErrorCode' ERROR_NO_SUCH_PACKAGE                                                    = Just "ERROR_NO_SUCH_PACKAGE"
displaySystemErrorCode' ERROR_BAD_LOGON_SESSION_STATE                                            = Just "ERROR_BAD_LOGON_SESSION_STATE"
displaySystemErrorCode' ERROR_LOGON_SESSION_COLLISION                                            = Just "ERROR_LOGON_SESSION_COLLISION"
displaySystemErrorCode' ERROR_INVALID_LOGON_TYPE                                                 = Just "ERROR_INVALID_LOGON_TYPE"
displaySystemErrorCode' ERROR_CANNOT_IMPERSONATE                                                 = Just "ERROR_CANNOT_IMPERSONATE"
displaySystemErrorCode' ERROR_RXACT_INVALID_STATE                                                = Just "ERROR_RXACT_INVALID_STATE"
displaySystemErrorCode' ERROR_RXACT_COMMIT_FAILURE                                               = Just "ERROR_RXACT_COMMIT_FAILURE"
displaySystemErrorCode' ERROR_SPECIAL_ACCOUNT                                                    = Just "ERROR_SPECIAL_ACCOUNT"
displaySystemErrorCode' ERROR_SPECIAL_GROUP                                                      = Just "ERROR_SPECIAL_GROUP"
displaySystemErrorCode' ERROR_SPECIAL_USER                                                       = Just "ERROR_SPECIAL_USER"
displaySystemErrorCode' ERROR_MEMBERS_PRIMARY_GROUP                                              = Just "ERROR_MEMBERS_PRIMARY_GROUP"
displaySystemErrorCode' ERROR_TOKEN_ALREADY_IN_USE                                               = Just "ERROR_TOKEN_ALREADY_IN_USE"
displaySystemErrorCode' ERROR_NO_SUCH_ALIAS                                                      = Just "ERROR_NO_SUCH_ALIAS"
displaySystemErrorCode' ERROR_MEMBER_NOT_IN_ALIAS                                                = Just "ERROR_MEMBER_NOT_IN_ALIAS"
displaySystemErrorCode' ERROR_MEMBER_IN_ALIAS                                                    = Just "ERROR_MEMBER_IN_ALIAS"
displaySystemErrorCode' ERROR_ALIAS_EXISTS                                                       = Just "ERROR_ALIAS_EXISTS"
displaySystemErrorCode' ERROR_LOGON_NOT_GRANTED                                                  = Just "ERROR_LOGON_NOT_GRANTED"
displaySystemErrorCode' ERROR_TOO_MANY_SECRETS                                                   = Just "ERROR_TOO_MANY_SECRETS"
displaySystemErrorCode' ERROR_SECRET_TOO_LONG                                                    = Just "ERROR_SECRET_TOO_LONG"
displaySystemErrorCode' ERROR_INTERNAL_DB_ERROR                                                  = Just "ERROR_INTERNAL_DB_ERROR"
displaySystemErrorCode' ERROR_TOO_MANY_CONTEXT_IDS                                               = Just "ERROR_TOO_MANY_CONTEXT_IDS"
displaySystemErrorCode' ERROR_LOGON_TYPE_NOT_GRANTED                                             = Just "ERROR_LOGON_TYPE_NOT_GRANTED"
displaySystemErrorCode' ERROR_NT_CROSS_ENCRYPTION_REQUIRED                                       = Just "ERROR_NT_CROSS_ENCRYPTION_REQUIRED"
displaySystemErrorCode' ERROR_NO_SUCH_MEMBER                                                     = Just "ERROR_NO_SUCH_MEMBER"
displaySystemErrorCode' ERROR_INVALID_MEMBER                                                     = Just "ERROR_INVALID_MEMBER"
displaySystemErrorCode' ERROR_TOO_MANY_SIDS                                                      = Just "ERROR_TOO_MANY_SIDS"
displaySystemErrorCode' ERROR_LM_CROSS_ENCRYPTION_REQUIRED                                       = Just "ERROR_LM_CROSS_ENCRYPTION_REQUIRED"
displaySystemErrorCode' ERROR_NO_INHERITANCE                                                     = Just "ERROR_NO_INHERITANCE"
displaySystemErrorCode' ERROR_FILE_CORRUPT                                                       = Just "ERROR_FILE_CORRUPT"
displaySystemErrorCode' ERROR_DISK_CORRUPT                                                       = Just "ERROR_DISK_CORRUPT"
displaySystemErrorCode' ERROR_NO_USER_SESSION_KEY                                                = Just "ERROR_NO_USER_SESSION_KEY"
displaySystemErrorCode' ERROR_LICENSE_QUOTA_EXCEEDED                                             = Just "ERROR_LICENSE_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_WRONG_TARGET_NAME                                                  = Just "ERROR_WRONG_TARGET_NAME"
displaySystemErrorCode' ERROR_MUTUAL_AUTH_FAILED                                                 = Just "ERROR_MUTUAL_AUTH_FAILED"
displaySystemErrorCode' ERROR_TIME_SKEW                                                          = Just "ERROR_TIME_SKEW"
displaySystemErrorCode' ERROR_CURRENT_DOMAIN_NOT_ALLOWED                                         = Just "ERROR_CURRENT_DOMAIN_NOT_ALLOWED"
displaySystemErrorCode' ERROR_INVALID_WINDOW_HANDLE                                              = Just "ERROR_INVALID_WINDOW_HANDLE"
displaySystemErrorCode' ERROR_INVALID_MENU_HANDLE                                                = Just "ERROR_INVALID_MENU_HANDLE"
displaySystemErrorCode' ERROR_INVALID_CURSOR_HANDLE                                              = Just "ERROR_INVALID_CURSOR_HANDLE"
displaySystemErrorCode' ERROR_INVALID_ACCEL_HANDLE                                               = Just "ERROR_INVALID_ACCEL_HANDLE"
displaySystemErrorCode' ERROR_INVALID_HOOK_HANDLE                                                = Just "ERROR_INVALID_HOOK_HANDLE"
displaySystemErrorCode' ERROR_INVALID_DWP_HANDLE                                                 = Just "ERROR_INVALID_DWP_HANDLE"
displaySystemErrorCode' ERROR_TLW_WITH_WSCHILD                                                   = Just "ERROR_TLW_WITH_WSCHILD"
displaySystemErrorCode' ERROR_CANNOT_FIND_WND_CLASS                                              = Just "ERROR_CANNOT_FIND_WND_CLASS"
displaySystemErrorCode' ERROR_WINDOW_OF_OTHER_THREAD                                             = Just "ERROR_WINDOW_OF_OTHER_THREAD"
displaySystemErrorCode' ERROR_HOTKEY_ALREADY_REGISTERED                                          = Just "ERROR_HOTKEY_ALREADY_REGISTERED"
displaySystemErrorCode' ERROR_CLASS_ALREADY_EXISTS                                               = Just "ERROR_CLASS_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_CLASS_DOES_NOT_EXIST                                               = Just "ERROR_CLASS_DOES_NOT_EXIST"
displaySystemErrorCode' ERROR_CLASS_HAS_WINDOWS                                                  = Just "ERROR_CLASS_HAS_WINDOWS"
displaySystemErrorCode' ERROR_INVALID_INDEX                                                      = Just "ERROR_INVALID_INDEX"
displaySystemErrorCode' ERROR_INVALID_ICON_HANDLE                                                = Just "ERROR_INVALID_ICON_HANDLE"
displaySystemErrorCode' ERROR_PRIVATE_DIALOG_INDEX                                               = Just "ERROR_PRIVATE_DIALOG_INDEX"
displaySystemErrorCode' ERROR_LISTBOX_ID_NOT_FOUND                                               = Just "ERROR_LISTBOX_ID_NOT_FOUND"
displaySystemErrorCode' ERROR_NO_WILDCARD_CHARACTERS                                             = Just "ERROR_NO_WILDCARD_CHARACTERS"
displaySystemErrorCode' ERROR_CLIPBOARD_NOT_OPEN                                                 = Just "ERROR_CLIPBOARD_NOT_OPEN"
displaySystemErrorCode' ERROR_HOTKEY_NOT_REGISTERED                                              = Just "ERROR_HOTKEY_NOT_REGISTERED"
displaySystemErrorCode' ERROR_WINDOW_NOT_DIALOG                                                  = Just "ERROR_WINDOW_NOT_DIALOG"
displaySystemErrorCode' ERROR_CONTROL_ID_NOT_FOUND                                               = Just "ERROR_CONTROL_ID_NOT_FOUND"
displaySystemErrorCode' ERROR_INVALID_COMBOBOX_MESSAGE                                           = Just "ERROR_INVALID_COMBOBOX_MESSAGE"
displaySystemErrorCode' ERROR_WINDOW_NOT_COMBOBOX                                                = Just "ERROR_WINDOW_NOT_COMBOBOX"
displaySystemErrorCode' ERROR_INVALID_EDIT_HEIGHT                                                = Just "ERROR_INVALID_EDIT_HEIGHT"
displaySystemErrorCode' ERROR_DC_NOT_FOUND                                                       = Just "ERROR_DC_NOT_FOUND"
displaySystemErrorCode' ERROR_INVALID_HOOK_FILTER                                                = Just "ERROR_INVALID_HOOK_FILTER"
displaySystemErrorCode' ERROR_INVALID_FILTER_PROC                                                = Just "ERROR_INVALID_FILTER_PROC"
displaySystemErrorCode' ERROR_HOOK_NEEDS_HMOD                                                    = Just "ERROR_HOOK_NEEDS_HMOD"
displaySystemErrorCode' ERROR_GLOBAL_ONLY_HOOK                                                   = Just "ERROR_GLOBAL_ONLY_HOOK"
displaySystemErrorCode' ERROR_JOURNAL_HOOK_SET                                                   = Just "ERROR_JOURNAL_HOOK_SET"
displaySystemErrorCode' ERROR_HOOK_NOT_INSTALLED                                                 = Just "ERROR_HOOK_NOT_INSTALLED"
displaySystemErrorCode' ERROR_INVALID_LB_MESSAGE                                                 = Just "ERROR_INVALID_LB_MESSAGE"
displaySystemErrorCode' ERROR_SETCOUNT_ON_BAD_LB                                                 = Just "ERROR_SETCOUNT_ON_BAD_LB"
displaySystemErrorCode' ERROR_LB_WITHOUT_TABSTOPS                                                = Just "ERROR_LB_WITHOUT_TABSTOPS"
displaySystemErrorCode' ERROR_DESTROY_OBJECT_OF_OTHER_THREAD                                     = Just "ERROR_DESTROY_OBJECT_OF_OTHER_THREAD"
displaySystemErrorCode' ERROR_CHILD_WINDOW_MENU                                                  = Just "ERROR_CHILD_WINDOW_MENU"
displaySystemErrorCode' ERROR_NO_SYSTEM_MENU                                                     = Just "ERROR_NO_SYSTEM_MENU"
displaySystemErrorCode' ERROR_INVALID_MSGBOX_STYLE                                               = Just "ERROR_INVALID_MSGBOX_STYLE"
displaySystemErrorCode' ERROR_INVALID_SPI_VALUE                                                  = Just "ERROR_INVALID_SPI_VALUE"
displaySystemErrorCode' ERROR_SCREEN_ALREADY_LOCKED                                              = Just "ERROR_SCREEN_ALREADY_LOCKED"
displaySystemErrorCode' ERROR_HWNDS_HAVE_DIFF_PARENT                                             = Just "ERROR_HWNDS_HAVE_DIFF_PARENT"
displaySystemErrorCode' ERROR_NOT_CHILD_WINDOW                                                   = Just "ERROR_NOT_CHILD_WINDOW"
displaySystemErrorCode' ERROR_INVALID_GW_COMMAND                                                 = Just "ERROR_INVALID_GW_COMMAND"
displaySystemErrorCode' ERROR_INVALID_THREAD_ID                                                  = Just "ERROR_INVALID_THREAD_ID"
displaySystemErrorCode' ERROR_NON_MDICHILD_WINDOW                                                = Just "ERROR_NON_MDICHILD_WINDOW"
displaySystemErrorCode' ERROR_POPUP_ALREADY_ACTIVE                                               = Just "ERROR_POPUP_ALREADY_ACTIVE"
displaySystemErrorCode' ERROR_NO_SCROLLBARS                                                      = Just "ERROR_NO_SCROLLBARS"
displaySystemErrorCode' ERROR_INVALID_SCROLLBAR_RANGE                                            = Just "ERROR_INVALID_SCROLLBAR_RANGE"
displaySystemErrorCode' ERROR_INVALID_SHOWWIN_COMMAND                                            = Just "ERROR_INVALID_SHOWWIN_COMMAND"
displaySystemErrorCode' ERROR_NO_SYSTEM_RESOURCES                                                = Just "ERROR_NO_SYSTEM_RESOURCES"
displaySystemErrorCode' ERROR_NONPAGED_SYSTEM_RESOURCES                                          = Just "ERROR_NONPAGED_SYSTEM_RESOURCES"
displaySystemErrorCode' ERROR_PAGED_SYSTEM_RESOURCES                                             = Just "ERROR_PAGED_SYSTEM_RESOURCES"
displaySystemErrorCode' ERROR_WORKING_SET_QUOTA                                                  = Just "ERROR_WORKING_SET_QUOTA"
displaySystemErrorCode' ERROR_PAGEFILE_QUOTA                                                     = Just "ERROR_PAGEFILE_QUOTA"
displaySystemErrorCode' ERROR_COMMITMENT_LIMIT                                                   = Just "ERROR_COMMITMENT_LIMIT"
displaySystemErrorCode' ERROR_MENU_ITEM_NOT_FOUND                                                = Just "ERROR_MENU_ITEM_NOT_FOUND"
displaySystemErrorCode' ERROR_INVALID_KEYBOARD_HANDLE                                            = Just "ERROR_INVALID_KEYBOARD_HANDLE"
displaySystemErrorCode' ERROR_HOOK_TYPE_NOT_ALLOWED                                              = Just "ERROR_HOOK_TYPE_NOT_ALLOWED"
displaySystemErrorCode' ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION                                 = Just "ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION"
displaySystemErrorCode' ERROR_TIMEOUT                                                            = Just "ERROR_TIMEOUT"
displaySystemErrorCode' ERROR_INVALID_MONITOR_HANDLE                                             = Just "ERROR_INVALID_MONITOR_HANDLE"
displaySystemErrorCode' ERROR_INCORRECT_SIZE                                                     = Just "ERROR_INCORRECT_SIZE"
displaySystemErrorCode' ERROR_SYMLINK_CLASS_DISABLED                                             = Just "ERROR_SYMLINK_CLASS_DISABLED"
displaySystemErrorCode' ERROR_SYMLINK_NOT_SUPPORTED                                              = Just "ERROR_SYMLINK_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_XML_PARSE_ERROR                                                    = Just "ERROR_XML_PARSE_ERROR"
displaySystemErrorCode' ERROR_XMLDSIG_ERROR                                                      = Just "ERROR_XMLDSIG_ERROR"
displaySystemErrorCode' ERROR_RESTART_APPLICATION                                                = Just "ERROR_RESTART_APPLICATION"
displaySystemErrorCode' ERROR_WRONG_COMPARTMENT                                                  = Just "ERROR_WRONG_COMPARTMENT"
displaySystemErrorCode' ERROR_AUTHIP_FAILURE                                                     = Just "ERROR_AUTHIP_FAILURE"
displaySystemErrorCode' ERROR_NO_NVRAM_RESOURCES                                                 = Just "ERROR_NO_NVRAM_RESOURCES"
displaySystemErrorCode' ERROR_NOT_GUI_PROCESS                                                    = Just "ERROR_NOT_GUI_PROCESS"
displaySystemErrorCode' ERROR_EVENTLOG_FILE_CORRUPT                                              = Just "ERROR_EVENTLOG_FILE_CORRUPT"
displaySystemErrorCode' ERROR_EVENTLOG_CANT_START                                                = Just "ERROR_EVENTLOG_CANT_START"
displaySystemErrorCode' ERROR_LOG_FILE_FULL                                                      = Just "ERROR_LOG_FILE_FULL"
displaySystemErrorCode' ERROR_EVENTLOG_FILE_CHANGED                                              = Just "ERROR_EVENTLOG_FILE_CHANGED"
displaySystemErrorCode' ERROR_INVALID_TASK_NAME                                                  = Just "ERROR_INVALID_TASK_NAME"
displaySystemErrorCode' ERROR_INVALID_TASK_INDEX                                                 = Just "ERROR_INVALID_TASK_INDEX"
displaySystemErrorCode' ERROR_THREAD_ALREADY_IN_TASK                                             = Just "ERROR_THREAD_ALREADY_IN_TASK"
displaySystemErrorCode' ERROR_INSTALL_SERVICE_FAILURE                                            = Just "ERROR_INSTALL_SERVICE_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_USEREXIT                                                   = Just "ERROR_INSTALL_USEREXIT"
displaySystemErrorCode' ERROR_INSTALL_FAILURE                                                    = Just "ERROR_INSTALL_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_SUSPEND                                                    = Just "ERROR_INSTALL_SUSPEND"
displaySystemErrorCode' ERROR_UNKNOWN_PRODUCT                                                    = Just "ERROR_UNKNOWN_PRODUCT"
displaySystemErrorCode' ERROR_UNKNOWN_FEATURE                                                    = Just "ERROR_UNKNOWN_FEATURE"
displaySystemErrorCode' ERROR_UNKNOWN_COMPONENT                                                  = Just "ERROR_UNKNOWN_COMPONENT"
displaySystemErrorCode' ERROR_UNKNOWN_PROPERTY                                                   = Just "ERROR_UNKNOWN_PROPERTY"
displaySystemErrorCode' ERROR_INVALID_HANDLE_STATE                                               = Just "ERROR_INVALID_HANDLE_STATE"
displaySystemErrorCode' ERROR_BAD_CONFIGURATION                                                  = Just "ERROR_BAD_CONFIGURATION"
displaySystemErrorCode' ERROR_INDEX_ABSENT                                                       = Just "ERROR_INDEX_ABSENT"
displaySystemErrorCode' ERROR_INSTALL_SOURCE_ABSENT                                              = Just "ERROR_INSTALL_SOURCE_ABSENT"
displaySystemErrorCode' ERROR_INSTALL_PACKAGE_VERSION                                            = Just "ERROR_INSTALL_PACKAGE_VERSION"
displaySystemErrorCode' ERROR_PRODUCT_UNINSTALLED                                                = Just "ERROR_PRODUCT_UNINSTALLED"
displaySystemErrorCode' ERROR_BAD_QUERY_SYNTAX                                                   = Just "ERROR_BAD_QUERY_SYNTAX"
displaySystemErrorCode' ERROR_INVALID_FIELD                                                      = Just "ERROR_INVALID_FIELD"
displaySystemErrorCode' ERROR_DEVICE_REMOVED                                                     = Just "ERROR_DEVICE_REMOVED"
displaySystemErrorCode' ERROR_INSTALL_ALREADY_RUNNING                                            = Just "ERROR_INSTALL_ALREADY_RUNNING"
displaySystemErrorCode' ERROR_INSTALL_PACKAGE_OPEN_FAILED                                        = Just "ERROR_INSTALL_PACKAGE_OPEN_FAILED"
displaySystemErrorCode' ERROR_INSTALL_PACKAGE_INVALID                                            = Just "ERROR_INSTALL_PACKAGE_INVALID"
displaySystemErrorCode' ERROR_INSTALL_UI_FAILURE                                                 = Just "ERROR_INSTALL_UI_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_LOG_FAILURE                                                = Just "ERROR_INSTALL_LOG_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_LANGUAGE_UNSUPPORTED                                       = Just "ERROR_INSTALL_LANGUAGE_UNSUPPORTED"
displaySystemErrorCode' ERROR_INSTALL_TRANSFORM_FAILURE                                          = Just "ERROR_INSTALL_TRANSFORM_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_PACKAGE_REJECTED                                           = Just "ERROR_INSTALL_PACKAGE_REJECTED"
displaySystemErrorCode' ERROR_FUNCTION_NOT_CALLED                                                = Just "ERROR_FUNCTION_NOT_CALLED"
displaySystemErrorCode' ERROR_FUNCTION_FAILED                                                    = Just "ERROR_FUNCTION_FAILED"
displaySystemErrorCode' ERROR_INVALID_TABLE                                                      = Just "ERROR_INVALID_TABLE"
displaySystemErrorCode' ERROR_DATATYPE_MISMATCH                                                  = Just "ERROR_DATATYPE_MISMATCH"
displaySystemErrorCode' ERROR_UNSUPPORTED_TYPE                                                   = Just "ERROR_UNSUPPORTED_TYPE"
displaySystemErrorCode' ERROR_CREATE_FAILED                                                      = Just "ERROR_CREATE_FAILED"
displaySystemErrorCode' ERROR_INSTALL_TEMP_UNWRITABLE                                            = Just "ERROR_INSTALL_TEMP_UNWRITABLE"
displaySystemErrorCode' ERROR_INSTALL_PLATFORM_UNSUPPORTED                                       = Just "ERROR_INSTALL_PLATFORM_UNSUPPORTED"
displaySystemErrorCode' ERROR_INSTALL_NOTUSED                                                    = Just "ERROR_INSTALL_NOTUSED"
displaySystemErrorCode' ERROR_PATCH_PACKAGE_OPEN_FAILED                                          = Just "ERROR_PATCH_PACKAGE_OPEN_FAILED"
displaySystemErrorCode' ERROR_PATCH_PACKAGE_INVALID                                              = Just "ERROR_PATCH_PACKAGE_INVALID"
displaySystemErrorCode' ERROR_PATCH_PACKAGE_UNSUPPORTED                                          = Just "ERROR_PATCH_PACKAGE_UNSUPPORTED"
displaySystemErrorCode' ERROR_PRODUCT_VERSION                                                    = Just "ERROR_PRODUCT_VERSION"
displaySystemErrorCode' ERROR_INVALID_COMMAND_LINE                                               = Just "ERROR_INVALID_COMMAND_LINE"
displaySystemErrorCode' ERROR_INSTALL_REMOTE_DISALLOWED                                          = Just "ERROR_INSTALL_REMOTE_DISALLOWED"
displaySystemErrorCode' ERROR_SUCCESS_REBOOT_INITIATED                                           = Just "ERROR_SUCCESS_REBOOT_INITIATED"
displaySystemErrorCode' ERROR_PATCH_TARGET_NOT_FOUND                                             = Just "ERROR_PATCH_TARGET_NOT_FOUND"
displaySystemErrorCode' ERROR_PATCH_PACKAGE_REJECTED                                             = Just "ERROR_PATCH_PACKAGE_REJECTED"
displaySystemErrorCode' ERROR_INSTALL_TRANSFORM_REJECTED                                         = Just "ERROR_INSTALL_TRANSFORM_REJECTED"
displaySystemErrorCode' ERROR_INSTALL_REMOTE_PROHIBITED                                          = Just "ERROR_INSTALL_REMOTE_PROHIBITED"
displaySystemErrorCode' ERROR_PATCH_REMOVAL_UNSUPPORTED                                          = Just "ERROR_PATCH_REMOVAL_UNSUPPORTED"
displaySystemErrorCode' ERROR_UNKNOWN_PATCH                                                      = Just "ERROR_UNKNOWN_PATCH"
displaySystemErrorCode' ERROR_PATCH_NO_SEQUENCE                                                  = Just "ERROR_PATCH_NO_SEQUENCE"
displaySystemErrorCode' ERROR_PATCH_REMOVAL_DISALLOWED                                           = Just "ERROR_PATCH_REMOVAL_DISALLOWED"
displaySystemErrorCode' ERROR_INVALID_PATCH_XML                                                  = Just "ERROR_INVALID_PATCH_XML"
displaySystemErrorCode' ERROR_PATCH_MANAGED_ADVERTISED_PRODUCT                                   = Just "ERROR_PATCH_MANAGED_ADVERTISED_PRODUCT"
displaySystemErrorCode' ERROR_INSTALL_SERVICE_SAFEBOOT                                           = Just "ERROR_INSTALL_SERVICE_SAFEBOOT"
displaySystemErrorCode' ERROR_FAIL_FAST_EXCEPTION                                                = Just "ERROR_FAIL_FAST_EXCEPTION"
displaySystemErrorCode' ERROR_INSTALL_REJECTED                                                   = Just "ERROR_INSTALL_REJECTED"
displaySystemErrorCode' ERROR_DYNAMIC_CODE_BLOCKED                                               = Just "ERROR_DYNAMIC_CODE_BLOCKED"
displaySystemErrorCode' ERROR_INVALID_USER_BUFFER                                                = Just "ERROR_INVALID_USER_BUFFER"
displaySystemErrorCode' ERROR_UNRECOGNIZED_MEDIA                                                 = Just "ERROR_UNRECOGNIZED_MEDIA"
displaySystemErrorCode' ERROR_NO_TRUST_LSA_SECRET                                                = Just "ERROR_NO_TRUST_LSA_SECRET"
displaySystemErrorCode' ERROR_NO_TRUST_SAM_ACCOUNT                                               = Just "ERROR_NO_TRUST_SAM_ACCOUNT"
displaySystemErrorCode' ERROR_TRUSTED_DOMAIN_FAILURE                                             = Just "ERROR_TRUSTED_DOMAIN_FAILURE"
displaySystemErrorCode' ERROR_TRUSTED_RELATIONSHIP_FAILURE                                       = Just "ERROR_TRUSTED_RELATIONSHIP_FAILURE"
displaySystemErrorCode' ERROR_TRUST_FAILURE                                                      = Just "ERROR_TRUST_FAILURE"
displaySystemErrorCode' ERROR_NETLOGON_NOT_STARTED                                               = Just "ERROR_NETLOGON_NOT_STARTED"
displaySystemErrorCode' ERROR_ACCOUNT_EXPIRED                                                    = Just "ERROR_ACCOUNT_EXPIRED"
displaySystemErrorCode' ERROR_REDIRECTOR_HAS_OPEN_HANDLES                                        = Just "ERROR_REDIRECTOR_HAS_OPEN_HANDLES"
displaySystemErrorCode' ERROR_PRINTER_DRIVER_ALREADY_INSTALLED                                   = Just "ERROR_PRINTER_DRIVER_ALREADY_INSTALLED"
displaySystemErrorCode' ERROR_UNKNOWN_PORT                                                       = Just "ERROR_UNKNOWN_PORT"
displaySystemErrorCode' ERROR_UNKNOWN_PRINTER_DRIVER                                             = Just "ERROR_UNKNOWN_PRINTER_DRIVER"
displaySystemErrorCode' ERROR_UNKNOWN_PRINTPROCESSOR                                             = Just "ERROR_UNKNOWN_PRINTPROCESSOR"
displaySystemErrorCode' ERROR_INVALID_SEPARATOR_FILE                                             = Just "ERROR_INVALID_SEPARATOR_FILE"
displaySystemErrorCode' ERROR_INVALID_PRIORITY                                                   = Just "ERROR_INVALID_PRIORITY"
displaySystemErrorCode' ERROR_INVALID_PRINTER_NAME                                               = Just "ERROR_INVALID_PRINTER_NAME"
displaySystemErrorCode' ERROR_PRINTER_ALREADY_EXISTS                                             = Just "ERROR_PRINTER_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_INVALID_PRINTER_COMMAND                                            = Just "ERROR_INVALID_PRINTER_COMMAND"
displaySystemErrorCode' ERROR_INVALID_DATATYPE                                                   = Just "ERROR_INVALID_DATATYPE"
displaySystemErrorCode' ERROR_INVALID_ENVIRONMENT                                                = Just "ERROR_INVALID_ENVIRONMENT"
displaySystemErrorCode' ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT                                  = Just "ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT"
displaySystemErrorCode' ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT                                  = Just "ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT"
displaySystemErrorCode' ERROR_NOLOGON_SERVER_TRUST_ACCOUNT                                       = Just "ERROR_NOLOGON_SERVER_TRUST_ACCOUNT"
displaySystemErrorCode' ERROR_DOMAIN_TRUST_INCONSISTENT                                          = Just "ERROR_DOMAIN_TRUST_INCONSISTENT"
displaySystemErrorCode' ERROR_SERVER_HAS_OPEN_HANDLES                                            = Just "ERROR_SERVER_HAS_OPEN_HANDLES"
displaySystemErrorCode' ERROR_RESOURCE_DATA_NOT_FOUND                                            = Just "ERROR_RESOURCE_DATA_NOT_FOUND"
displaySystemErrorCode' ERROR_RESOURCE_TYPE_NOT_FOUND                                            = Just "ERROR_RESOURCE_TYPE_NOT_FOUND"
displaySystemErrorCode' ERROR_RESOURCE_NAME_NOT_FOUND                                            = Just "ERROR_RESOURCE_NAME_NOT_FOUND"
displaySystemErrorCode' ERROR_RESOURCE_LANG_NOT_FOUND                                            = Just "ERROR_RESOURCE_LANG_NOT_FOUND"
displaySystemErrorCode' ERROR_NOT_ENOUGH_QUOTA                                                   = Just "ERROR_NOT_ENOUGH_QUOTA"
displaySystemErrorCode' ERROR_INVALID_TIME                                                       = Just "ERROR_INVALID_TIME"
displaySystemErrorCode' ERROR_INVALID_FORM_NAME                                                  = Just "ERROR_INVALID_FORM_NAME"
displaySystemErrorCode' ERROR_INVALID_FORM_SIZE                                                  = Just "ERROR_INVALID_FORM_SIZE"
displaySystemErrorCode' ERROR_ALREADY_WAITING                                                    = Just "ERROR_ALREADY_WAITING"
displaySystemErrorCode' ERROR_PRINTER_DELETED                                                    = Just "ERROR_PRINTER_DELETED"
displaySystemErrorCode' ERROR_INVALID_PRINTER_STATE                                              = Just "ERROR_INVALID_PRINTER_STATE"
displaySystemErrorCode' ERROR_PASSWORD_MUST_CHANGE                                               = Just "ERROR_PASSWORD_MUST_CHANGE"
displaySystemErrorCode' ERROR_DOMAIN_CONTROLLER_NOT_FOUND                                        = Just "ERROR_DOMAIN_CONTROLLER_NOT_FOUND"
displaySystemErrorCode' ERROR_ACCOUNT_LOCKED_OUT                                                 = Just "ERROR_ACCOUNT_LOCKED_OUT"
displaySystemErrorCode' ERROR_NO_SITENAME                                                        = Just "ERROR_NO_SITENAME"
displaySystemErrorCode' ERROR_CANT_ACCESS_FILE                                                   = Just "ERROR_CANT_ACCESS_FILE"
displaySystemErrorCode' ERROR_CANT_RESOLVE_FILENAME                                              = Just "ERROR_CANT_RESOLVE_FILENAME"
displaySystemErrorCode' ERROR_KM_DRIVER_BLOCKED                                                  = Just "ERROR_KM_DRIVER_BLOCKED"
displaySystemErrorCode' ERROR_CONTEXT_EXPIRED                                                    = Just "ERROR_CONTEXT_EXPIRED"
displaySystemErrorCode' ERROR_PER_USER_TRUST_QUOTA_EXCEEDED                                      = Just "ERROR_PER_USER_TRUST_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_ALL_USER_TRUST_QUOTA_EXCEEDED                                      = Just "ERROR_ALL_USER_TRUST_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_USER_DELETE_TRUST_QUOTA_EXCEEDED                                   = Just "ERROR_USER_DELETE_TRUST_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_AUTHENTICATION_FIREWALL_FAILED                                     = Just "ERROR_AUTHENTICATION_FIREWALL_FAILED"
displaySystemErrorCode' ERROR_REMOTE_PRINT_CONNECTIONS_BLOCKED                                   = Just "ERROR_REMOTE_PRINT_CONNECTIONS_BLOCKED"
displaySystemErrorCode' ERROR_NTLM_BLOCKED                                                       = Just "ERROR_NTLM_BLOCKED"
displaySystemErrorCode' ERROR_PASSWORD_CHANGE_REQUIRED                                           = Just "ERROR_PASSWORD_CHANGE_REQUIRED"
displaySystemErrorCode' ERROR_INVALID_PIXEL_FORMAT                                               = Just "ERROR_INVALID_PIXEL_FORMAT"
displaySystemErrorCode' ERROR_BAD_DRIVER                                                         = Just "ERROR_BAD_DRIVER"
displaySystemErrorCode' ERROR_INVALID_WINDOW_STYLE                                               = Just "ERROR_INVALID_WINDOW_STYLE"
displaySystemErrorCode' ERROR_METAFILE_NOT_SUPPORTED                                             = Just "ERROR_METAFILE_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_TRANSFORM_NOT_SUPPORTED                                            = Just "ERROR_TRANSFORM_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_CLIPPING_NOT_SUPPORTED                                             = Just "ERROR_CLIPPING_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_INVALID_CMM                                                        = Just "ERROR_INVALID_CMM"
displaySystemErrorCode' ERROR_INVALID_PROFILE                                                    = Just "ERROR_INVALID_PROFILE"
displaySystemErrorCode' ERROR_TAG_NOT_FOUND                                                      = Just "ERROR_TAG_NOT_FOUND"
displaySystemErrorCode' ERROR_TAG_NOT_PRESENT                                                    = Just "ERROR_TAG_NOT_PRESENT"
displaySystemErrorCode' ERROR_DUPLICATE_TAG                                                      = Just "ERROR_DUPLICATE_TAG"
displaySystemErrorCode' ERROR_PROFILE_NOT_ASSOCIATED_WITH_DEVICE                                 = Just "ERROR_PROFILE_NOT_ASSOCIATED_WITH_DEVICE"
displaySystemErrorCode' ERROR_PROFILE_NOT_FOUND                                                  = Just "ERROR_PROFILE_NOT_FOUND"
displaySystemErrorCode' ERROR_INVALID_COLORSPACE                                                 = Just "ERROR_INVALID_COLORSPACE"
displaySystemErrorCode' ERROR_ICM_NOT_ENABLED                                                    = Just "ERROR_ICM_NOT_ENABLED"
displaySystemErrorCode' ERROR_DELETING_ICM_XFORM                                                 = Just "ERROR_DELETING_ICM_XFORM"
displaySystemErrorCode' ERROR_INVALID_TRANSFORM                                                  = Just "ERROR_INVALID_TRANSFORM"
displaySystemErrorCode' ERROR_COLORSPACE_MISMATCH                                                = Just "ERROR_COLORSPACE_MISMATCH"
displaySystemErrorCode' ERROR_INVALID_COLORINDEX                                                 = Just "ERROR_INVALID_COLORINDEX"
displaySystemErrorCode' ERROR_PROFILE_DOES_NOT_MATCH_DEVICE                                      = Just "ERROR_PROFILE_DOES_NOT_MATCH_DEVICE"
displaySystemErrorCode' ERROR_CONNECTED_OTHER_PASSWORD                                           = Just "ERROR_CONNECTED_OTHER_PASSWORD"
displaySystemErrorCode' ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT                                   = Just "ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT"
displaySystemErrorCode' ERROR_BAD_USERNAME                                                       = Just "ERROR_BAD_USERNAME"
displaySystemErrorCode' ERROR_NOT_CONNECTED                                                      = Just "ERROR_NOT_CONNECTED"
displaySystemErrorCode' ERROR_OPEN_FILES                                                         = Just "ERROR_OPEN_FILES"
displaySystemErrorCode' ERROR_ACTIVE_CONNECTIONS                                                 = Just "ERROR_ACTIVE_CONNECTIONS"
displaySystemErrorCode' ERROR_DEVICE_IN_USE                                                      = Just "ERROR_DEVICE_IN_USE"
displaySystemErrorCode' ERROR_UNKNOWN_PRINT_MONITOR                                              = Just "ERROR_UNKNOWN_PRINT_MONITOR"
displaySystemErrorCode' ERROR_PRINTER_DRIVER_IN_USE                                              = Just "ERROR_PRINTER_DRIVER_IN_USE"
displaySystemErrorCode' ERROR_SPOOL_FILE_NOT_FOUND                                               = Just "ERROR_SPOOL_FILE_NOT_FOUND"
displaySystemErrorCode' ERROR_SPL_NO_STARTDOC                                                    = Just "ERROR_SPL_NO_STARTDOC"
displaySystemErrorCode' ERROR_SPL_NO_ADDJOB                                                      = Just "ERROR_SPL_NO_ADDJOB"
displaySystemErrorCode' ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED                                  = Just "ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED"
displaySystemErrorCode' ERROR_PRINT_MONITOR_ALREADY_INSTALLED                                    = Just "ERROR_PRINT_MONITOR_ALREADY_INSTALLED"
displaySystemErrorCode' ERROR_INVALID_PRINT_MONITOR                                              = Just "ERROR_INVALID_PRINT_MONITOR"
displaySystemErrorCode' ERROR_PRINT_MONITOR_IN_USE                                               = Just "ERROR_PRINT_MONITOR_IN_USE"
displaySystemErrorCode' ERROR_PRINTER_HAS_JOBS_QUEUED                                            = Just "ERROR_PRINTER_HAS_JOBS_QUEUED"
displaySystemErrorCode' ERROR_SUCCESS_REBOOT_REQUIRED                                            = Just "ERROR_SUCCESS_REBOOT_REQUIRED"
displaySystemErrorCode' ERROR_SUCCESS_RESTART_REQUIRED                                           = Just "ERROR_SUCCESS_RESTART_REQUIRED"
displaySystemErrorCode' ERROR_PRINTER_NOT_FOUND                                                  = Just "ERROR_PRINTER_NOT_FOUND"
displaySystemErrorCode' ERROR_PRINTER_DRIVER_WARNED                                              = Just "ERROR_PRINTER_DRIVER_WARNED"
displaySystemErrorCode' ERROR_PRINTER_DRIVER_BLOCKED                                             = Just "ERROR_PRINTER_DRIVER_BLOCKED"
displaySystemErrorCode' ERROR_PRINTER_DRIVER_PACKAGE_IN_USE                                      = Just "ERROR_PRINTER_DRIVER_PACKAGE_IN_USE"
displaySystemErrorCode' ERROR_CORE_DRIVER_PACKAGE_NOT_FOUND                                      = Just "ERROR_CORE_DRIVER_PACKAGE_NOT_FOUND"
displaySystemErrorCode' ERROR_FAIL_REBOOT_REQUIRED                                               = Just "ERROR_FAIL_REBOOT_REQUIRED"
displaySystemErrorCode' ERROR_FAIL_REBOOT_INITIATED                                              = Just "ERROR_FAIL_REBOOT_INITIATED"
displaySystemErrorCode' ERROR_PRINTER_DRIVER_DOWNLOAD_NEEDED                                     = Just "ERROR_PRINTER_DRIVER_DOWNLOAD_NEEDED"
displaySystemErrorCode' ERROR_PRINT_JOB_RESTART_REQUIRED                                         = Just "ERROR_PRINT_JOB_RESTART_REQUIRED"
displaySystemErrorCode' ERROR_INVALID_PRINTER_DRIVER_MANIFEST                                    = Just "ERROR_INVALID_PRINTER_DRIVER_MANIFEST"
displaySystemErrorCode' ERROR_PRINTER_NOT_SHAREABLE                                              = Just "ERROR_PRINTER_NOT_SHAREABLE"
displaySystemErrorCode' ERROR_REQUEST_PAUSED                                                     = Just "ERROR_REQUEST_PAUSED"
displaySystemErrorCode' ERROR_IO_REISSUE_AS_CACHED                                               = Just "ERROR_IO_REISSUE_AS_CACHED"
displaySystemErrorCode' ERROR_WINS_INTERNAL                                                      = Just "ERROR_WINS_INTERNAL"
displaySystemErrorCode' ERROR_CAN_NOT_DEL_LOCAL_WINS                                             = Just "ERROR_CAN_NOT_DEL_LOCAL_WINS"
displaySystemErrorCode' ERROR_STATIC_INIT                                                        = Just "ERROR_STATIC_INIT"
displaySystemErrorCode' ERROR_INC_BACKUP                                                         = Just "ERROR_INC_BACKUP"
displaySystemErrorCode' ERROR_FULL_BACKUP                                                        = Just "ERROR_FULL_BACKUP"
displaySystemErrorCode' ERROR_REC_NON_EXISTENT                                                   = Just "ERROR_REC_NON_EXISTENT"
displaySystemErrorCode' ERROR_RPL_NOT_ALLOWED                                                    = Just "ERROR_RPL_NOT_ALLOWED"
displaySystemErrorCode' ERROR_DHCP_ADDRESS_CONFLICT                                              = Just "ERROR_DHCP_ADDRESS_CONFLICT"
displaySystemErrorCode' ERROR_WMI_GUID_NOT_FOUND                                                 = Just "ERROR_WMI_GUID_NOT_FOUND"
displaySystemErrorCode' ERROR_WMI_INSTANCE_NOT_FOUND                                             = Just "ERROR_WMI_INSTANCE_NOT_FOUND"
displaySystemErrorCode' ERROR_WMI_ITEMID_NOT_FOUND                                               = Just "ERROR_WMI_ITEMID_NOT_FOUND"
displaySystemErrorCode' ERROR_WMI_TRY_AGAIN                                                      = Just "ERROR_WMI_TRY_AGAIN"
displaySystemErrorCode' ERROR_WMI_DP_NOT_FOUND                                                   = Just "ERROR_WMI_DP_NOT_FOUND"
displaySystemErrorCode' ERROR_WMI_UNRESOLVED_INSTANCE_REF                                        = Just "ERROR_WMI_UNRESOLVED_INSTANCE_REF"
displaySystemErrorCode' ERROR_WMI_ALREADY_ENABLED                                                = Just "ERROR_WMI_ALREADY_ENABLED"
displaySystemErrorCode' ERROR_WMI_GUID_DISCONNECTED                                              = Just "ERROR_WMI_GUID_DISCONNECTED"
displaySystemErrorCode' ERROR_WMI_SERVER_UNAVAILABLE                                             = Just "ERROR_WMI_SERVER_UNAVAILABLE"
displaySystemErrorCode' ERROR_WMI_DP_FAILED                                                      = Just "ERROR_WMI_DP_FAILED"
displaySystemErrorCode' ERROR_WMI_INVALID_MOF                                                    = Just "ERROR_WMI_INVALID_MOF"
displaySystemErrorCode' ERROR_WMI_INVALID_REGINFO                                                = Just "ERROR_WMI_INVALID_REGINFO"
displaySystemErrorCode' ERROR_WMI_ALREADY_DISABLED                                               = Just "ERROR_WMI_ALREADY_DISABLED"
displaySystemErrorCode' ERROR_WMI_READ_ONLY                                                      = Just "ERROR_WMI_READ_ONLY"
displaySystemErrorCode' ERROR_WMI_SET_FAILURE                                                    = Just "ERROR_WMI_SET_FAILURE"
displaySystemErrorCode' ERROR_NOT_APPCONTAINER                                                   = Just "ERROR_NOT_APPCONTAINER"
displaySystemErrorCode' ERROR_APPCONTAINER_REQUIRED                                              = Just "ERROR_APPCONTAINER_REQUIRED"
displaySystemErrorCode' ERROR_NOT_SUPPORTED_IN_APPCONTAINER                                      = Just "ERROR_NOT_SUPPORTED_IN_APPCONTAINER"
displaySystemErrorCode' ERROR_INVALID_PACKAGE_SID_LENGTH                                         = Just "ERROR_INVALID_PACKAGE_SID_LENGTH"
displaySystemErrorCode' ERROR_INVALID_MEDIA                                                      = Just "ERROR_INVALID_MEDIA"
displaySystemErrorCode' ERROR_INVALID_LIBRARY                                                    = Just "ERROR_INVALID_LIBRARY"
displaySystemErrorCode' ERROR_INVALID_MEDIA_POOL                                                 = Just "ERROR_INVALID_MEDIA_POOL"
displaySystemErrorCode' ERROR_DRIVE_MEDIA_MISMATCH                                               = Just "ERROR_DRIVE_MEDIA_MISMATCH"
displaySystemErrorCode' ERROR_MEDIA_OFFLINE                                                      = Just "ERROR_MEDIA_OFFLINE"
displaySystemErrorCode' ERROR_LIBRARY_OFFLINE                                                    = Just "ERROR_LIBRARY_OFFLINE"
displaySystemErrorCode' ERROR_EMPTY                                                              = Just "ERROR_EMPTY"
displaySystemErrorCode' ERROR_NOT_EMPTY                                                          = Just "ERROR_NOT_EMPTY"
displaySystemErrorCode' ERROR_MEDIA_UNAVAILABLE                                                  = Just "ERROR_MEDIA_UNAVAILABLE"
displaySystemErrorCode' ERROR_RESOURCE_DISABLED                                                  = Just "ERROR_RESOURCE_DISABLED"
displaySystemErrorCode' ERROR_INVALID_CLEANER                                                    = Just "ERROR_INVALID_CLEANER"
displaySystemErrorCode' ERROR_UNABLE_TO_CLEAN                                                    = Just "ERROR_UNABLE_TO_CLEAN"
displaySystemErrorCode' ERROR_OBJECT_NOT_FOUND                                                   = Just "ERROR_OBJECT_NOT_FOUND"
displaySystemErrorCode' ERROR_DATABASE_FAILURE                                                   = Just "ERROR_DATABASE_FAILURE"
displaySystemErrorCode' ERROR_DATABASE_FULL                                                      = Just "ERROR_DATABASE_FULL"
displaySystemErrorCode' ERROR_MEDIA_INCOMPATIBLE                                                 = Just "ERROR_MEDIA_INCOMPATIBLE"
displaySystemErrorCode' ERROR_RESOURCE_NOT_PRESENT                                               = Just "ERROR_RESOURCE_NOT_PRESENT"
displaySystemErrorCode' ERROR_INVALID_OPERATION                                                  = Just "ERROR_INVALID_OPERATION"
displaySystemErrorCode' ERROR_MEDIA_NOT_AVAILABLE                                                = Just "ERROR_MEDIA_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_DEVICE_NOT_AVAILABLE                                               = Just "ERROR_DEVICE_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_REQUEST_REFUSED                                                    = Just "ERROR_REQUEST_REFUSED"
displaySystemErrorCode' ERROR_INVALID_DRIVE_OBJECT                                               = Just "ERROR_INVALID_DRIVE_OBJECT"
displaySystemErrorCode' ERROR_LIBRARY_FULL                                                       = Just "ERROR_LIBRARY_FULL"
displaySystemErrorCode' ERROR_MEDIUM_NOT_ACCESSIBLE                                              = Just "ERROR_MEDIUM_NOT_ACCESSIBLE"
displaySystemErrorCode' ERROR_UNABLE_TO_LOAD_MEDIUM                                              = Just "ERROR_UNABLE_TO_LOAD_MEDIUM"
displaySystemErrorCode' ERROR_UNABLE_TO_INVENTORY_DRIVE                                          = Just "ERROR_UNABLE_TO_INVENTORY_DRIVE"
displaySystemErrorCode' ERROR_UNABLE_TO_INVENTORY_SLOT                                           = Just "ERROR_UNABLE_TO_INVENTORY_SLOT"
displaySystemErrorCode' ERROR_UNABLE_TO_INVENTORY_TRANSPORT                                      = Just "ERROR_UNABLE_TO_INVENTORY_TRANSPORT"
displaySystemErrorCode' ERROR_TRANSPORT_FULL                                                     = Just "ERROR_TRANSPORT_FULL"
displaySystemErrorCode' ERROR_CONTROLLING_IEPORT                                                 = Just "ERROR_CONTROLLING_IEPORT"
displaySystemErrorCode' ERROR_UNABLE_TO_EJECT_MOUNTED_MEDIA                                      = Just "ERROR_UNABLE_TO_EJECT_MOUNTED_MEDIA"
displaySystemErrorCode' ERROR_CLEANER_SLOT_SET                                                   = Just "ERROR_CLEANER_SLOT_SET"
displaySystemErrorCode' ERROR_CLEANER_SLOT_NOT_SET                                               = Just "ERROR_CLEANER_SLOT_NOT_SET"
displaySystemErrorCode' ERROR_CLEANER_CARTRIDGE_SPENT                                            = Just "ERROR_CLEANER_CARTRIDGE_SPENT"
displaySystemErrorCode' ERROR_UNEXPECTED_OMID                                                    = Just "ERROR_UNEXPECTED_OMID"
displaySystemErrorCode' ERROR_CANT_DELETE_LAST_ITEM                                              = Just "ERROR_CANT_DELETE_LAST_ITEM"
displaySystemErrorCode' ERROR_MESSAGE_EXCEEDS_MAX_SIZE                                           = Just "ERROR_MESSAGE_EXCEEDS_MAX_SIZE"
displaySystemErrorCode' ERROR_VOLUME_CONTAINS_SYS_FILES                                          = Just "ERROR_VOLUME_CONTAINS_SYS_FILES"
displaySystemErrorCode' ERROR_INDIGENOUS_TYPE                                                    = Just "ERROR_INDIGENOUS_TYPE"
displaySystemErrorCode' ERROR_NO_SUPPORTING_DRIVES                                               = Just "ERROR_NO_SUPPORTING_DRIVES"
displaySystemErrorCode' ERROR_CLEANER_CARTRIDGE_INSTALLED                                        = Just "ERROR_CLEANER_CARTRIDGE_INSTALLED"
displaySystemErrorCode' ERROR_IEPORT_FULL                                                        = Just "ERROR_IEPORT_FULL"
displaySystemErrorCode' ERROR_FILE_OFFLINE                                                       = Just "ERROR_FILE_OFFLINE"
displaySystemErrorCode' ERROR_REMOTE_STORAGE_NOT_ACTIVE                                          = Just "ERROR_REMOTE_STORAGE_NOT_ACTIVE"
displaySystemErrorCode' ERROR_REMOTE_STORAGE_MEDIA_ERROR                                         = Just "ERROR_REMOTE_STORAGE_MEDIA_ERROR"
displaySystemErrorCode' ERROR_NOT_A_REPARSE_POINT                                                = Just "ERROR_NOT_A_REPARSE_POINT"
displaySystemErrorCode' ERROR_REPARSE_ATTRIBUTE_CONFLICT                                         = Just "ERROR_REPARSE_ATTRIBUTE_CONFLICT"
displaySystemErrorCode' ERROR_INVALID_REPARSE_DATA                                               = Just "ERROR_INVALID_REPARSE_DATA"
displaySystemErrorCode' ERROR_REPARSE_TAG_INVALID                                                = Just "ERROR_REPARSE_TAG_INVALID"
displaySystemErrorCode' ERROR_REPARSE_TAG_MISMATCH                                               = Just "ERROR_REPARSE_TAG_MISMATCH"
displaySystemErrorCode' ERROR_APP_DATA_NOT_FOUND                                                 = Just "ERROR_APP_DATA_NOT_FOUND"
displaySystemErrorCode' ERROR_APP_DATA_EXPIRED                                                   = Just "ERROR_APP_DATA_EXPIRED"
displaySystemErrorCode' ERROR_APP_DATA_CORRUPT                                                   = Just "ERROR_APP_DATA_CORRUPT"
displaySystemErrorCode' ERROR_APP_DATA_LIMIT_EXCEEDED                                            = Just "ERROR_APP_DATA_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_APP_DATA_REBOOT_REQUIRED                                           = Just "ERROR_APP_DATA_REBOOT_REQUIRED"
displaySystemErrorCode' ERROR_SECUREBOOT_ROLLBACK_DETECTED                                       = Just "ERROR_SECUREBOOT_ROLLBACK_DETECTED"
displaySystemErrorCode' ERROR_SECUREBOOT_POLICY_VIOLATION                                        = Just "ERROR_SECUREBOOT_POLICY_VIOLATION"
displaySystemErrorCode' ERROR_SECUREBOOT_INVALID_POLICY                                          = Just "ERROR_SECUREBOOT_INVALID_POLICY"
displaySystemErrorCode' ERROR_SECUREBOOT_POLICY_PUBLISHER_NOT_FOUND                              = Just "ERROR_SECUREBOOT_POLICY_PUBLISHER_NOT_FOUND"
displaySystemErrorCode' ERROR_SECUREBOOT_POLICY_NOT_SIGNED                                       = Just "ERROR_SECUREBOOT_POLICY_NOT_SIGNED"
displaySystemErrorCode' ERROR_SECUREBOOT_NOT_ENABLED                                             = Just "ERROR_SECUREBOOT_NOT_ENABLED"
displaySystemErrorCode' ERROR_SECUREBOOT_FILE_REPLACED                                           = Just "ERROR_SECUREBOOT_FILE_REPLACED"
displaySystemErrorCode' ERROR_OFFLOAD_READ_FLT_NOT_SUPPORTED                                     = Just "ERROR_OFFLOAD_READ_FLT_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_OFFLOAD_WRITE_FLT_NOT_SUPPORTED                                    = Just "ERROR_OFFLOAD_WRITE_FLT_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_OFFLOAD_READ_FILE_NOT_SUPPORTED                                    = Just "ERROR_OFFLOAD_READ_FILE_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_OFFLOAD_WRITE_FILE_NOT_SUPPORTED                                   = Just "ERROR_OFFLOAD_WRITE_FILE_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_VOLUME_NOT_SIS_ENABLED                                             = Just "ERROR_VOLUME_NOT_SIS_ENABLED"
displaySystemErrorCode' ERROR_DEPENDENT_RESOURCE_EXISTS                                          = Just "ERROR_DEPENDENT_RESOURCE_EXISTS"
displaySystemErrorCode' ERROR_DEPENDENCY_NOT_FOUND                                               = Just "ERROR_DEPENDENCY_NOT_FOUND"
displaySystemErrorCode' ERROR_DEPENDENCY_ALREADY_EXISTS                                          = Just "ERROR_DEPENDENCY_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_RESOURCE_NOT_ONLINE                                                = Just "ERROR_RESOURCE_NOT_ONLINE"
displaySystemErrorCode' ERROR_HOST_NODE_NOT_AVAILABLE                                            = Just "ERROR_HOST_NODE_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_RESOURCE_NOT_AVAILABLE                                             = Just "ERROR_RESOURCE_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_RESOURCE_NOT_FOUND                                                 = Just "ERROR_RESOURCE_NOT_FOUND"
displaySystemErrorCode' ERROR_SHUTDOWN_CLUSTER                                                   = Just "ERROR_SHUTDOWN_CLUSTER"
displaySystemErrorCode' ERROR_CANT_EVICT_ACTIVE_NODE                                             = Just "ERROR_CANT_EVICT_ACTIVE_NODE"
displaySystemErrorCode' ERROR_OBJECT_ALREADY_EXISTS                                              = Just "ERROR_OBJECT_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_OBJECT_IN_LIST                                                     = Just "ERROR_OBJECT_IN_LIST"
displaySystemErrorCode' ERROR_GROUP_NOT_AVAILABLE                                                = Just "ERROR_GROUP_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_GROUP_NOT_FOUND                                                    = Just "ERROR_GROUP_NOT_FOUND"
displaySystemErrorCode' ERROR_GROUP_NOT_ONLINE                                                   = Just "ERROR_GROUP_NOT_ONLINE"
displaySystemErrorCode' ERROR_HOST_NODE_NOT_RESOURCE_OWNER                                       = Just "ERROR_HOST_NODE_NOT_RESOURCE_OWNER"
displaySystemErrorCode' ERROR_HOST_NODE_NOT_GROUP_OWNER                                          = Just "ERROR_HOST_NODE_NOT_GROUP_OWNER"
displaySystemErrorCode' ERROR_RESMON_CREATE_FAILED                                               = Just "ERROR_RESMON_CREATE_FAILED"
displaySystemErrorCode' ERROR_RESMON_ONLINE_FAILED                                               = Just "ERROR_RESMON_ONLINE_FAILED"
displaySystemErrorCode' ERROR_RESOURCE_ONLINE                                                    = Just "ERROR_RESOURCE_ONLINE"
displaySystemErrorCode' ERROR_QUORUM_RESOURCE                                                    = Just "ERROR_QUORUM_RESOURCE"
displaySystemErrorCode' ERROR_NOT_QUORUM_CAPABLE                                                 = Just "ERROR_NOT_QUORUM_CAPABLE"
displaySystemErrorCode' ERROR_CLUSTER_SHUTTING_DOWN                                              = Just "ERROR_CLUSTER_SHUTTING_DOWN"
displaySystemErrorCode' ERROR_INVALID_STATE                                                      = Just "ERROR_INVALID_STATE"
displaySystemErrorCode' ERROR_RESOURCE_PROPERTIES_STORED                                         = Just "ERROR_RESOURCE_PROPERTIES_STORED"
displaySystemErrorCode' ERROR_NOT_QUORUM_CLASS                                                   = Just "ERROR_NOT_QUORUM_CLASS"
displaySystemErrorCode' ERROR_CORE_RESOURCE                                                      = Just "ERROR_CORE_RESOURCE"
displaySystemErrorCode' ERROR_QUORUM_RESOURCE_ONLINE_FAILED                                      = Just "ERROR_QUORUM_RESOURCE_ONLINE_FAILED"
displaySystemErrorCode' ERROR_QUORUMLOG_OPEN_FAILED                                              = Just "ERROR_QUORUMLOG_OPEN_FAILED"
displaySystemErrorCode' ERROR_CLUSTERLOG_CORRUPT                                                 = Just "ERROR_CLUSTERLOG_CORRUPT"
displaySystemErrorCode' ERROR_CLUSTERLOG_RECORD_EXCEEDS_MAXSIZE                                  = Just "ERROR_CLUSTERLOG_RECORD_EXCEEDS_MAXSIZE"
displaySystemErrorCode' ERROR_CLUSTERLOG_EXCEEDS_MAXSIZE                                         = Just "ERROR_CLUSTERLOG_EXCEEDS_MAXSIZE"
displaySystemErrorCode' ERROR_CLUSTERLOG_CHKPOINT_NOT_FOUND                                      = Just "ERROR_CLUSTERLOG_CHKPOINT_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTERLOG_NOT_ENOUGH_SPACE                                        = Just "ERROR_CLUSTERLOG_NOT_ENOUGH_SPACE"
displaySystemErrorCode' ERROR_QUORUM_OWNER_ALIVE                                                 = Just "ERROR_QUORUM_OWNER_ALIVE"
displaySystemErrorCode' ERROR_NETWORK_NOT_AVAILABLE                                              = Just "ERROR_NETWORK_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_NODE_NOT_AVAILABLE                                                 = Just "ERROR_NODE_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_ALL_NODES_NOT_AVAILABLE                                            = Just "ERROR_ALL_NODES_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_RESOURCE_FAILED                                                    = Just "ERROR_RESOURCE_FAILED"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_NODE                                               = Just "ERROR_CLUSTER_INVALID_NODE"
displaySystemErrorCode' ERROR_CLUSTER_NODE_EXISTS                                                = Just "ERROR_CLUSTER_NODE_EXISTS"
displaySystemErrorCode' ERROR_CLUSTER_JOIN_IN_PROGRESS                                           = Just "ERROR_CLUSTER_JOIN_IN_PROGRESS"
displaySystemErrorCode' ERROR_CLUSTER_NODE_NOT_FOUND                                             = Just "ERROR_CLUSTER_NODE_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTER_LOCAL_NODE_NOT_FOUND                                       = Just "ERROR_CLUSTER_LOCAL_NODE_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTER_NETWORK_EXISTS                                             = Just "ERROR_CLUSTER_NETWORK_EXISTS"
displaySystemErrorCode' ERROR_CLUSTER_NETWORK_NOT_FOUND                                          = Just "ERROR_CLUSTER_NETWORK_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTER_NETINTERFACE_EXISTS                                        = Just "ERROR_CLUSTER_NETINTERFACE_EXISTS"
displaySystemErrorCode' ERROR_CLUSTER_NETINTERFACE_NOT_FOUND                                     = Just "ERROR_CLUSTER_NETINTERFACE_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_REQUEST                                            = Just "ERROR_CLUSTER_INVALID_REQUEST"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_NETWORK_PROVIDER                                   = Just "ERROR_CLUSTER_INVALID_NETWORK_PROVIDER"
displaySystemErrorCode' ERROR_CLUSTER_NODE_DOWN                                                  = Just "ERROR_CLUSTER_NODE_DOWN"
displaySystemErrorCode' ERROR_CLUSTER_NODE_UNREACHABLE                                           = Just "ERROR_CLUSTER_NODE_UNREACHABLE"
displaySystemErrorCode' ERROR_CLUSTER_NODE_NOT_MEMBER                                            = Just "ERROR_CLUSTER_NODE_NOT_MEMBER"
displaySystemErrorCode' ERROR_CLUSTER_JOIN_NOT_IN_PROGRESS                                       = Just "ERROR_CLUSTER_JOIN_NOT_IN_PROGRESS"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_NETWORK                                            = Just "ERROR_CLUSTER_INVALID_NETWORK"
displaySystemErrorCode' ERROR_CLUSTER_NODE_UP                                                    = Just "ERROR_CLUSTER_NODE_UP"
displaySystemErrorCode' ERROR_CLUSTER_IPADDR_IN_USE                                              = Just "ERROR_CLUSTER_IPADDR_IN_USE"
displaySystemErrorCode' ERROR_CLUSTER_NODE_NOT_PAUSED                                            = Just "ERROR_CLUSTER_NODE_NOT_PAUSED"
displaySystemErrorCode' ERROR_CLUSTER_NO_SECURITY_CONTEXT                                        = Just "ERROR_CLUSTER_NO_SECURITY_CONTEXT"
displaySystemErrorCode' ERROR_CLUSTER_NETWORK_NOT_INTERNAL                                       = Just "ERROR_CLUSTER_NETWORK_NOT_INTERNAL"
displaySystemErrorCode' ERROR_CLUSTER_NODE_ALREADY_UP                                            = Just "ERROR_CLUSTER_NODE_ALREADY_UP"
displaySystemErrorCode' ERROR_CLUSTER_NODE_ALREADY_DOWN                                          = Just "ERROR_CLUSTER_NODE_ALREADY_DOWN"
displaySystemErrorCode' ERROR_CLUSTER_NETWORK_ALREADY_ONLINE                                     = Just "ERROR_CLUSTER_NETWORK_ALREADY_ONLINE"
displaySystemErrorCode' ERROR_CLUSTER_NETWORK_ALREADY_OFFLINE                                    = Just "ERROR_CLUSTER_NETWORK_ALREADY_OFFLINE"
displaySystemErrorCode' ERROR_CLUSTER_NODE_ALREADY_MEMBER                                        = Just "ERROR_CLUSTER_NODE_ALREADY_MEMBER"
displaySystemErrorCode' ERROR_CLUSTER_LAST_INTERNAL_NETWORK                                      = Just "ERROR_CLUSTER_LAST_INTERNAL_NETWORK"
displaySystemErrorCode' ERROR_CLUSTER_NETWORK_HAS_DEPENDENTS                                     = Just "ERROR_CLUSTER_NETWORK_HAS_DEPENDENTS"
displaySystemErrorCode' ERROR_INVALID_OPERATION_ON_QUORUM                                        = Just "ERROR_INVALID_OPERATION_ON_QUORUM"
displaySystemErrorCode' ERROR_DEPENDENCY_NOT_ALLOWED                                             = Just "ERROR_DEPENDENCY_NOT_ALLOWED"
displaySystemErrorCode' ERROR_CLUSTER_NODE_PAUSED                                                = Just "ERROR_CLUSTER_NODE_PAUSED"
displaySystemErrorCode' ERROR_NODE_CANT_HOST_RESOURCE                                            = Just "ERROR_NODE_CANT_HOST_RESOURCE"
displaySystemErrorCode' ERROR_CLUSTER_NODE_NOT_READY                                             = Just "ERROR_CLUSTER_NODE_NOT_READY"
displaySystemErrorCode' ERROR_CLUSTER_NODE_SHUTTING_DOWN                                         = Just "ERROR_CLUSTER_NODE_SHUTTING_DOWN"
displaySystemErrorCode' ERROR_CLUSTER_JOIN_ABORTED                                               = Just "ERROR_CLUSTER_JOIN_ABORTED"
displaySystemErrorCode' ERROR_CLUSTER_INCOMPATIBLE_VERSIONS                                      = Just "ERROR_CLUSTER_INCOMPATIBLE_VERSIONS"
displaySystemErrorCode' ERROR_CLUSTER_MAXNUM_OF_RESOURCES_EXCEEDED                               = Just "ERROR_CLUSTER_MAXNUM_OF_RESOURCES_EXCEEDED"
displaySystemErrorCode' ERROR_CLUSTER_SYSTEM_CONFIG_CHANGED                                      = Just "ERROR_CLUSTER_SYSTEM_CONFIG_CHANGED"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_TYPE_NOT_FOUND                                    = Just "ERROR_CLUSTER_RESOURCE_TYPE_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTER_RESTYPE_NOT_SUPPORTED                                      = Just "ERROR_CLUSTER_RESTYPE_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_CLUSTER_RESNAME_NOT_FOUND                                          = Just "ERROR_CLUSTER_RESNAME_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTER_NO_RPC_PACKAGES_REGISTERED                                 = Just "ERROR_CLUSTER_NO_RPC_PACKAGES_REGISTERED"
displaySystemErrorCode' ERROR_CLUSTER_OWNER_NOT_IN_PREFLIST                                      = Just "ERROR_CLUSTER_OWNER_NOT_IN_PREFLIST"
displaySystemErrorCode' ERROR_CLUSTER_DATABASE_SEQMISMATCH                                       = Just "ERROR_CLUSTER_DATABASE_SEQMISMATCH"
displaySystemErrorCode' ERROR_RESMON_INVALID_STATE                                               = Just "ERROR_RESMON_INVALID_STATE"
displaySystemErrorCode' ERROR_CLUSTER_GUM_NOT_LOCKER                                             = Just "ERROR_CLUSTER_GUM_NOT_LOCKER"
displaySystemErrorCode' ERROR_QUORUM_DISK_NOT_FOUND                                              = Just "ERROR_QUORUM_DISK_NOT_FOUND"
displaySystemErrorCode' ERROR_DATABASE_BACKUP_CORRUPT                                            = Just "ERROR_DATABASE_BACKUP_CORRUPT"
displaySystemErrorCode' ERROR_CLUSTER_NODE_ALREADY_HAS_DFS_ROOT                                  = Just "ERROR_CLUSTER_NODE_ALREADY_HAS_DFS_ROOT"
displaySystemErrorCode' ERROR_RESOURCE_PROPERTY_UNCHANGEABLE                                     = Just "ERROR_RESOURCE_PROPERTY_UNCHANGEABLE"
displaySystemErrorCode' ERROR_NO_ADMIN_ACCESS_POINT                                              = Just "ERROR_NO_ADMIN_ACCESS_POINT"
displaySystemErrorCode' ERROR_CLUSTER_MEMBERSHIP_INVALID_STATE                                   = Just "ERROR_CLUSTER_MEMBERSHIP_INVALID_STATE"
displaySystemErrorCode' ERROR_CLUSTER_QUORUMLOG_NOT_FOUND                                        = Just "ERROR_CLUSTER_QUORUMLOG_NOT_FOUND"
displaySystemErrorCode' ERROR_CLUSTER_MEMBERSHIP_HALT                                            = Just "ERROR_CLUSTER_MEMBERSHIP_HALT"
displaySystemErrorCode' ERROR_CLUSTER_INSTANCE_ID_MISMATCH                                       = Just "ERROR_CLUSTER_INSTANCE_ID_MISMATCH"
displaySystemErrorCode' ERROR_CLUSTER_NETWORK_NOT_FOUND_FOR_IP                                   = Just "ERROR_CLUSTER_NETWORK_NOT_FOUND_FOR_IP"
displaySystemErrorCode' ERROR_CLUSTER_PROPERTY_DATA_TYPE_MISMATCH                                = Just "ERROR_CLUSTER_PROPERTY_DATA_TYPE_MISMATCH"
displaySystemErrorCode' ERROR_CLUSTER_EVICT_WITHOUT_CLEANUP                                      = Just "ERROR_CLUSTER_EVICT_WITHOUT_CLEANUP"
displaySystemErrorCode' ERROR_CLUSTER_PARAMETER_MISMATCH                                         = Just "ERROR_CLUSTER_PARAMETER_MISMATCH"
displaySystemErrorCode' ERROR_NODE_CANNOT_BE_CLUSTERED                                           = Just "ERROR_NODE_CANNOT_BE_CLUSTERED"
displaySystemErrorCode' ERROR_CLUSTER_WRONG_OS_VERSION                                           = Just "ERROR_CLUSTER_WRONG_OS_VERSION"
displaySystemErrorCode' ERROR_CLUSTER_CANT_CREATE_DUP_CLUSTER_NAME                               = Just "ERROR_CLUSTER_CANT_CREATE_DUP_CLUSTER_NAME"
displaySystemErrorCode' ERROR_CLUSCFG_ALREADY_COMMITTED                                          = Just "ERROR_CLUSCFG_ALREADY_COMMITTED"
displaySystemErrorCode' ERROR_CLUSCFG_ROLLBACK_FAILED                                            = Just "ERROR_CLUSCFG_ROLLBACK_FAILED"
displaySystemErrorCode' ERROR_CLUSCFG_SYSTEM_DISK_DRIVE_LETTER_CONFLICT                          = Just "ERROR_CLUSCFG_SYSTEM_DISK_DRIVE_LETTER_CONFLICT"
displaySystemErrorCode' ERROR_CLUSTER_OLD_VERSION                                                = Just "ERROR_CLUSTER_OLD_VERSION"
displaySystemErrorCode' ERROR_CLUSTER_MISMATCHED_COMPUTER_ACCT_NAME                              = Just "ERROR_CLUSTER_MISMATCHED_COMPUTER_ACCT_NAME"
displaySystemErrorCode' ERROR_CLUSTER_NO_NET_ADAPTERS                                            = Just "ERROR_CLUSTER_NO_NET_ADAPTERS"
displaySystemErrorCode' ERROR_CLUSTER_POISONED                                                   = Just "ERROR_CLUSTER_POISONED"
displaySystemErrorCode' ERROR_CLUSTER_GROUP_MOVING                                               = Just "ERROR_CLUSTER_GROUP_MOVING"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_TYPE_BUSY                                         = Just "ERROR_CLUSTER_RESOURCE_TYPE_BUSY"
displaySystemErrorCode' ERROR_RESOURCE_CALL_TIMED_OUT                                            = Just "ERROR_RESOURCE_CALL_TIMED_OUT"
displaySystemErrorCode' ERROR_INVALID_CLUSTER_IPV6_ADDRESS                                       = Just "ERROR_INVALID_CLUSTER_IPV6_ADDRESS"
displaySystemErrorCode' ERROR_CLUSTER_INTERNAL_INVALID_FUNCTION                                  = Just "ERROR_CLUSTER_INTERNAL_INVALID_FUNCTION"
displaySystemErrorCode' ERROR_CLUSTER_PARAMETER_OUT_OF_BOUNDS                                    = Just "ERROR_CLUSTER_PARAMETER_OUT_OF_BOUNDS"
displaySystemErrorCode' ERROR_CLUSTER_PARTIAL_SEND                                               = Just "ERROR_CLUSTER_PARTIAL_SEND"
displaySystemErrorCode' ERROR_CLUSTER_REGISTRY_INVALID_FUNCTION                                  = Just "ERROR_CLUSTER_REGISTRY_INVALID_FUNCTION"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_STRING_TERMINATION                                 = Just "ERROR_CLUSTER_INVALID_STRING_TERMINATION"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_STRING_FORMAT                                      = Just "ERROR_CLUSTER_INVALID_STRING_FORMAT"
displaySystemErrorCode' ERROR_CLUSTER_DATABASE_TRANSACTION_IN_PROGRESS                           = Just "ERROR_CLUSTER_DATABASE_TRANSACTION_IN_PROGRESS"
displaySystemErrorCode' ERROR_CLUSTER_DATABASE_TRANSACTION_NOT_IN_PROGRESS                       = Just "ERROR_CLUSTER_DATABASE_TRANSACTION_NOT_IN_PROGRESS"
displaySystemErrorCode' ERROR_CLUSTER_NULL_DATA                                                  = Just "ERROR_CLUSTER_NULL_DATA"
displaySystemErrorCode' ERROR_CLUSTER_PARTIAL_READ                                               = Just "ERROR_CLUSTER_PARTIAL_READ"
displaySystemErrorCode' ERROR_CLUSTER_PARTIAL_WRITE                                              = Just "ERROR_CLUSTER_PARTIAL_WRITE"
displaySystemErrorCode' ERROR_CLUSTER_CANT_DESERIALIZE_DATA                                      = Just "ERROR_CLUSTER_CANT_DESERIALIZE_DATA"
displaySystemErrorCode' ERROR_DEPENDENT_RESOURCE_PROPERTY_CONFLICT                               = Just "ERROR_DEPENDENT_RESOURCE_PROPERTY_CONFLICT"
displaySystemErrorCode' ERROR_CLUSTER_NO_QUORUM                                                  = Just "ERROR_CLUSTER_NO_QUORUM"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_IPV6_NETWORK                                       = Just "ERROR_CLUSTER_INVALID_IPV6_NETWORK"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_IPV6_TUNNEL_NETWORK                                = Just "ERROR_CLUSTER_INVALID_IPV6_TUNNEL_NETWORK"
displaySystemErrorCode' ERROR_QUORUM_NOT_ALLOWED_IN_THIS_GROUP                                   = Just "ERROR_QUORUM_NOT_ALLOWED_IN_THIS_GROUP"
displaySystemErrorCode' ERROR_DEPENDENCY_TREE_TOO_COMPLEX                                        = Just "ERROR_DEPENDENCY_TREE_TOO_COMPLEX"
displaySystemErrorCode' ERROR_EXCEPTION_IN_RESOURCE_CALL                                         = Just "ERROR_EXCEPTION_IN_RESOURCE_CALL"
displaySystemErrorCode' ERROR_CLUSTER_RHS_FAILED_INITIALIZATION                                  = Just "ERROR_CLUSTER_RHS_FAILED_INITIALIZATION"
displaySystemErrorCode' ERROR_CLUSTER_NOT_INSTALLED                                              = Just "ERROR_CLUSTER_NOT_INSTALLED"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCES_MUST_BE_ONLINE_ON_THE_SAME_NODE                  = Just "ERROR_CLUSTER_RESOURCES_MUST_BE_ONLINE_ON_THE_SAME_NODE"
displaySystemErrorCode' ERROR_CLUSTER_MAX_NODES_IN_CLUSTER                                       = Just "ERROR_CLUSTER_MAX_NODES_IN_CLUSTER"
displaySystemErrorCode' ERROR_CLUSTER_TOO_MANY_NODES                                             = Just "ERROR_CLUSTER_TOO_MANY_NODES"
displaySystemErrorCode' ERROR_CLUSTER_OBJECT_ALREADY_USED                                        = Just "ERROR_CLUSTER_OBJECT_ALREADY_USED"
displaySystemErrorCode' ERROR_NONCORE_GROUPS_FOUND                                               = Just "ERROR_NONCORE_GROUPS_FOUND"
displaySystemErrorCode' ERROR_FILE_SHARE_RESOURCE_CONFLICT                                       = Just "ERROR_FILE_SHARE_RESOURCE_CONFLICT"
displaySystemErrorCode' ERROR_CLUSTER_EVICT_INVALID_REQUEST                                      = Just "ERROR_CLUSTER_EVICT_INVALID_REQUEST"
displaySystemErrorCode' ERROR_CLUSTER_SINGLETON_RESOURCE                                         = Just "ERROR_CLUSTER_SINGLETON_RESOURCE"
displaySystemErrorCode' ERROR_CLUSTER_GROUP_SINGLETON_RESOURCE                                   = Just "ERROR_CLUSTER_GROUP_SINGLETON_RESOURCE"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_PROVIDER_FAILED                                   = Just "ERROR_CLUSTER_RESOURCE_PROVIDER_FAILED"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_CONFIGURATION_ERROR                               = Just "ERROR_CLUSTER_RESOURCE_CONFIGURATION_ERROR"
displaySystemErrorCode' ERROR_CLUSTER_GROUP_BUSY                                                 = Just "ERROR_CLUSTER_GROUP_BUSY"
displaySystemErrorCode' ERROR_CLUSTER_NOT_SHARED_VOLUME                                          = Just "ERROR_CLUSTER_NOT_SHARED_VOLUME"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_SECURITY_DESCRIPTOR                                = Just "ERROR_CLUSTER_INVALID_SECURITY_DESCRIPTOR"
displaySystemErrorCode' ERROR_CLUSTER_SHARED_VOLUMES_IN_USE                                      = Just "ERROR_CLUSTER_SHARED_VOLUMES_IN_USE"
displaySystemErrorCode' ERROR_CLUSTER_USE_SHARED_VOLUMES_API                                     = Just "ERROR_CLUSTER_USE_SHARED_VOLUMES_API"
displaySystemErrorCode' ERROR_CLUSTER_BACKUP_IN_PROGRESS                                         = Just "ERROR_CLUSTER_BACKUP_IN_PROGRESS"
displaySystemErrorCode' ERROR_NON_CSV_PATH                                                       = Just "ERROR_NON_CSV_PATH"
displaySystemErrorCode' ERROR_CSV_VOLUME_NOT_LOCAL                                               = Just "ERROR_CSV_VOLUME_NOT_LOCAL"
displaySystemErrorCode' ERROR_CLUSTER_WATCHDOG_TERMINATING                                       = Just "ERROR_CLUSTER_WATCHDOG_TERMINATING"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_VETOED_MOVE_INCOMPATIBLE_NODES                    = Just "ERROR_CLUSTER_RESOURCE_VETOED_MOVE_INCOMPATIBLE_NODES"
displaySystemErrorCode' ERROR_CLUSTER_INVALID_NODE_WEIGHT                                        = Just "ERROR_CLUSTER_INVALID_NODE_WEIGHT"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_VETOED_CALL                                       = Just "ERROR_CLUSTER_RESOURCE_VETOED_CALL"
displaySystemErrorCode' ERROR_RESMON_SYSTEM_RESOURCES_LACKING                                    = Just "ERROR_RESMON_SYSTEM_RESOURCES_LACKING"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_VETOED_MOVE_NOT_ENOUGH_RESOURCES_ON_DESTINATION   = Just "ERROR_CLUSTER_RESOURCE_VETOED_MOVE_NOT_ENOUGH_RESOURCES_ON_DESTINATION"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_VETOED_MOVE_NOT_ENOUGH_RESOURCES_ON_SOURCE        = Just "ERROR_CLUSTER_RESOURCE_VETOED_MOVE_NOT_ENOUGH_RESOURCES_ON_SOURCE"
displaySystemErrorCode' ERROR_CLUSTER_GROUP_QUEUED                                               = Just "ERROR_CLUSTER_GROUP_QUEUED"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_LOCKED_STATUS                                     = Just "ERROR_CLUSTER_RESOURCE_LOCKED_STATUS"
displaySystemErrorCode' ERROR_CLUSTER_SHARED_VOLUME_FAILOVER_NOT_ALLOWED                         = Just "ERROR_CLUSTER_SHARED_VOLUME_FAILOVER_NOT_ALLOWED"
displaySystemErrorCode' ERROR_CLUSTER_NODE_DRAIN_IN_PROGRESS                                     = Just "ERROR_CLUSTER_NODE_DRAIN_IN_PROGRESS"
displaySystemErrorCode' ERROR_CLUSTER_DISK_NOT_CONNECTED                                         = Just "ERROR_CLUSTER_DISK_NOT_CONNECTED"
displaySystemErrorCode' ERROR_DISK_NOT_CSV_CAPABLE                                               = Just "ERROR_DISK_NOT_CSV_CAPABLE"
displaySystemErrorCode' ERROR_RESOURCE_NOT_IN_AVAILABLE_STORAGE                                  = Just "ERROR_RESOURCE_NOT_IN_AVAILABLE_STORAGE"
displaySystemErrorCode' ERROR_CLUSTER_SHARED_VOLUME_REDIRECTED                                   = Just "ERROR_CLUSTER_SHARED_VOLUME_REDIRECTED"
displaySystemErrorCode' ERROR_CLUSTER_SHARED_VOLUME_NOT_REDIRECTED                               = Just "ERROR_CLUSTER_SHARED_VOLUME_NOT_REDIRECTED"
displaySystemErrorCode' ERROR_CLUSTER_CANNOT_RETURN_PROPERTIES                                   = Just "ERROR_CLUSTER_CANNOT_RETURN_PROPERTIES"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_CONTAINS_UNSUPPORTED_DIFF_AREA_FOR_SHARED_VOLUMES = Just "ERROR_CLUSTER_RESOURCE_CONTAINS_UNSUPPORTED_DIFF_AREA_FOR_SHARED_VOLUMES"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_IS_IN_MAINTENANCE_MODE                            = Just "ERROR_CLUSTER_RESOURCE_IS_IN_MAINTENANCE_MODE"
displaySystemErrorCode' ERROR_CLUSTER_AFFINITY_CONFLICT                                          = Just "ERROR_CLUSTER_AFFINITY_CONFLICT"
displaySystemErrorCode' ERROR_CLUSTER_RESOURCE_IS_REPLICA_VIRTUAL_MACHINE                        = Just "ERROR_CLUSTER_RESOURCE_IS_REPLICA_VIRTUAL_MACHINE"
displaySystemErrorCode' ERROR_ENCRYPTION_FAILED                                                  = Just "ERROR_ENCRYPTION_FAILED"
displaySystemErrorCode' ERROR_DECRYPTION_FAILED                                                  = Just "ERROR_DECRYPTION_FAILED"
displaySystemErrorCode' ERROR_FILE_ENCRYPTED                                                     = Just "ERROR_FILE_ENCRYPTED"
displaySystemErrorCode' ERROR_NO_RECOVERY_POLICY                                                 = Just "ERROR_NO_RECOVERY_POLICY"
displaySystemErrorCode' ERROR_NO_EFS                                                             = Just "ERROR_NO_EFS"
displaySystemErrorCode' ERROR_WRONG_EFS                                                          = Just "ERROR_WRONG_EFS"
displaySystemErrorCode' ERROR_NO_USER_KEYS                                                       = Just "ERROR_NO_USER_KEYS"
displaySystemErrorCode' ERROR_FILE_NOT_ENCRYPTED                                                 = Just "ERROR_FILE_NOT_ENCRYPTED"
displaySystemErrorCode' ERROR_NOT_EXPORT_FORMAT                                                  = Just "ERROR_NOT_EXPORT_FORMAT"
displaySystemErrorCode' ERROR_FILE_READ_ONLY                                                     = Just "ERROR_FILE_READ_ONLY"
displaySystemErrorCode' ERROR_DIR_EFS_DISALLOWED                                                 = Just "ERROR_DIR_EFS_DISALLOWED"
displaySystemErrorCode' ERROR_EFS_SERVER_NOT_TRUSTED                                             = Just "ERROR_EFS_SERVER_NOT_TRUSTED"
displaySystemErrorCode' ERROR_BAD_RECOVERY_POLICY                                                = Just "ERROR_BAD_RECOVERY_POLICY"
displaySystemErrorCode' ERROR_EFS_ALG_BLOB_TOO_BIG                                               = Just "ERROR_EFS_ALG_BLOB_TOO_BIG"
displaySystemErrorCode' ERROR_VOLUME_NOT_SUPPORT_EFS                                             = Just "ERROR_VOLUME_NOT_SUPPORT_EFS"
displaySystemErrorCode' ERROR_EFS_DISABLED                                                       = Just "ERROR_EFS_DISABLED"
displaySystemErrorCode' ERROR_EFS_VERSION_NOT_SUPPORT                                            = Just "ERROR_EFS_VERSION_NOT_SUPPORT"
displaySystemErrorCode' ERROR_CS_ENCRYPTION_INVALID_SERVER_RESPONSE                              = Just "ERROR_CS_ENCRYPTION_INVALID_SERVER_RESPONSE"
displaySystemErrorCode' ERROR_CS_ENCRYPTION_UNSUPPORTED_SERVER                                   = Just "ERROR_CS_ENCRYPTION_UNSUPPORTED_SERVER"
displaySystemErrorCode' ERROR_CS_ENCRYPTION_EXISTING_ENCRYPTED_FILE                              = Just "ERROR_CS_ENCRYPTION_EXISTING_ENCRYPTED_FILE"
displaySystemErrorCode' ERROR_CS_ENCRYPTION_NEW_ENCRYPTED_FILE                                   = Just "ERROR_CS_ENCRYPTION_NEW_ENCRYPTED_FILE"
displaySystemErrorCode' ERROR_CS_ENCRYPTION_FILE_NOT_CSE                                         = Just "ERROR_CS_ENCRYPTION_FILE_NOT_CSE"
displaySystemErrorCode' ERROR_ENCRYPTION_POLICY_DENIES_OPERATION                                 = Just "ERROR_ENCRYPTION_POLICY_DENIES_OPERATION"
displaySystemErrorCode' ERROR_NO_BROWSER_SERVERS_FOUND                                           = Just "ERROR_NO_BROWSER_SERVERS_FOUND"
displaySystemErrorCode' ERROR_LOG_SECTOR_INVALID                                                 = Just "ERROR_LOG_SECTOR_INVALID"
displaySystemErrorCode' ERROR_LOG_SECTOR_PARITY_INVALID                                          = Just "ERROR_LOG_SECTOR_PARITY_INVALID"
displaySystemErrorCode' ERROR_LOG_SECTOR_REMAPPED                                                = Just "ERROR_LOG_SECTOR_REMAPPED"
displaySystemErrorCode' ERROR_LOG_BLOCK_INCOMPLETE                                               = Just "ERROR_LOG_BLOCK_INCOMPLETE"
displaySystemErrorCode' ERROR_LOG_INVALID_RANGE                                                  = Just "ERROR_LOG_INVALID_RANGE"
displaySystemErrorCode' ERROR_LOG_BLOCKS_EXHAUSTED                                               = Just "ERROR_LOG_BLOCKS_EXHAUSTED"
displaySystemErrorCode' ERROR_LOG_READ_CONTEXT_INVALID                                           = Just "ERROR_LOG_READ_CONTEXT_INVALID"
displaySystemErrorCode' ERROR_LOG_RESTART_INVALID                                                = Just "ERROR_LOG_RESTART_INVALID"
displaySystemErrorCode' ERROR_LOG_BLOCK_VERSION                                                  = Just "ERROR_LOG_BLOCK_VERSION"
displaySystemErrorCode' ERROR_LOG_BLOCK_INVALID                                                  = Just "ERROR_LOG_BLOCK_INVALID"
displaySystemErrorCode' ERROR_LOG_READ_MODE_INVALID                                              = Just "ERROR_LOG_READ_MODE_INVALID"
displaySystemErrorCode' ERROR_LOG_NO_RESTART                                                     = Just "ERROR_LOG_NO_RESTART"
displaySystemErrorCode' ERROR_LOG_METADATA_CORRUPT                                               = Just "ERROR_LOG_METADATA_CORRUPT"
displaySystemErrorCode' ERROR_LOG_METADATA_INVALID                                               = Just "ERROR_LOG_METADATA_INVALID"
displaySystemErrorCode' ERROR_LOG_METADATA_INCONSISTENT                                          = Just "ERROR_LOG_METADATA_INCONSISTENT"
displaySystemErrorCode' ERROR_LOG_RESERVATION_INVALID                                            = Just "ERROR_LOG_RESERVATION_INVALID"
displaySystemErrorCode' ERROR_LOG_CANT_DELETE                                                    = Just "ERROR_LOG_CANT_DELETE"
displaySystemErrorCode' ERROR_LOG_CONTAINER_LIMIT_EXCEEDED                                       = Just "ERROR_LOG_CONTAINER_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_LOG_START_OF_LOG                                                   = Just "ERROR_LOG_START_OF_LOG"
displaySystemErrorCode' ERROR_LOG_POLICY_ALREADY_INSTALLED                                       = Just "ERROR_LOG_POLICY_ALREADY_INSTALLED"
displaySystemErrorCode' ERROR_LOG_POLICY_NOT_INSTALLED                                           = Just "ERROR_LOG_POLICY_NOT_INSTALLED"
displaySystemErrorCode' ERROR_LOG_POLICY_INVALID                                                 = Just "ERROR_LOG_POLICY_INVALID"
displaySystemErrorCode' ERROR_LOG_POLICY_CONFLICT                                                = Just "ERROR_LOG_POLICY_CONFLICT"
displaySystemErrorCode' ERROR_LOG_PINNED_ARCHIVE_TAIL                                            = Just "ERROR_LOG_PINNED_ARCHIVE_TAIL"
displaySystemErrorCode' ERROR_LOG_RECORD_NONEXISTENT                                             = Just "ERROR_LOG_RECORD_NONEXISTENT"
displaySystemErrorCode' ERROR_LOG_RECORDS_RESERVED_INVALID                                       = Just "ERROR_LOG_RECORDS_RESERVED_INVALID"
displaySystemErrorCode' ERROR_LOG_SPACE_RESERVED_INVALID                                         = Just "ERROR_LOG_SPACE_RESERVED_INVALID"
displaySystemErrorCode' ERROR_LOG_TAIL_INVALID                                                   = Just "ERROR_LOG_TAIL_INVALID"
displaySystemErrorCode' ERROR_LOG_FULL                                                           = Just "ERROR_LOG_FULL"
displaySystemErrorCode' ERROR_COULD_NOT_RESIZE_LOG                                               = Just "ERROR_COULD_NOT_RESIZE_LOG"
displaySystemErrorCode' ERROR_LOG_MULTIPLEXED                                                    = Just "ERROR_LOG_MULTIPLEXED"
displaySystemErrorCode' ERROR_LOG_DEDICATED                                                      = Just "ERROR_LOG_DEDICATED"
displaySystemErrorCode' ERROR_LOG_ARCHIVE_NOT_IN_PROGRESS                                        = Just "ERROR_LOG_ARCHIVE_NOT_IN_PROGRESS"
displaySystemErrorCode' ERROR_LOG_ARCHIVE_IN_PROGRESS                                            = Just "ERROR_LOG_ARCHIVE_IN_PROGRESS"
displaySystemErrorCode' ERROR_LOG_EPHEMERAL                                                      = Just "ERROR_LOG_EPHEMERAL"
displaySystemErrorCode' ERROR_LOG_NOT_ENOUGH_CONTAINERS                                          = Just "ERROR_LOG_NOT_ENOUGH_CONTAINERS"
displaySystemErrorCode' ERROR_LOG_CLIENT_ALREADY_REGISTERED                                      = Just "ERROR_LOG_CLIENT_ALREADY_REGISTERED"
displaySystemErrorCode' ERROR_LOG_CLIENT_NOT_REGISTERED                                          = Just "ERROR_LOG_CLIENT_NOT_REGISTERED"
displaySystemErrorCode' ERROR_LOG_FULL_HANDLER_IN_PROGRESS                                       = Just "ERROR_LOG_FULL_HANDLER_IN_PROGRESS"
displaySystemErrorCode' ERROR_LOG_CONTAINER_READ_FAILED                                          = Just "ERROR_LOG_CONTAINER_READ_FAILED"
displaySystemErrorCode' ERROR_LOG_CONTAINER_WRITE_FAILED                                         = Just "ERROR_LOG_CONTAINER_WRITE_FAILED"
displaySystemErrorCode' ERROR_LOG_CONTAINER_OPEN_FAILED                                          = Just "ERROR_LOG_CONTAINER_OPEN_FAILED"
displaySystemErrorCode' ERROR_LOG_CONTAINER_STATE_INVALID                                        = Just "ERROR_LOG_CONTAINER_STATE_INVALID"
displaySystemErrorCode' ERROR_LOG_STATE_INVALID                                                  = Just "ERROR_LOG_STATE_INVALID"
displaySystemErrorCode' ERROR_LOG_PINNED                                                         = Just "ERROR_LOG_PINNED"
displaySystemErrorCode' ERROR_LOG_METADATA_FLUSH_FAILED                                          = Just "ERROR_LOG_METADATA_FLUSH_FAILED"
displaySystemErrorCode' ERROR_LOG_INCONSISTENT_SECURITY                                          = Just "ERROR_LOG_INCONSISTENT_SECURITY"
displaySystemErrorCode' ERROR_LOG_APPENDED_FLUSH_FAILED                                          = Just "ERROR_LOG_APPENDED_FLUSH_FAILED"
displaySystemErrorCode' ERROR_LOG_PINNED_RESERVATION                                             = Just "ERROR_LOG_PINNED_RESERVATION"
displaySystemErrorCode' ERROR_INVALID_TRANSACTION                                                = Just "ERROR_INVALID_TRANSACTION"
displaySystemErrorCode' ERROR_TRANSACTION_NOT_ACTIVE                                             = Just "ERROR_TRANSACTION_NOT_ACTIVE"
displaySystemErrorCode' ERROR_TRANSACTION_REQUEST_NOT_VALID                                      = Just "ERROR_TRANSACTION_REQUEST_NOT_VALID"
displaySystemErrorCode' ERROR_TRANSACTION_NOT_REQUESTED                                          = Just "ERROR_TRANSACTION_NOT_REQUESTED"
displaySystemErrorCode' ERROR_TRANSACTION_ALREADY_ABORTED                                        = Just "ERROR_TRANSACTION_ALREADY_ABORTED"
displaySystemErrorCode' ERROR_TRANSACTION_ALREADY_COMMITTED                                      = Just "ERROR_TRANSACTION_ALREADY_COMMITTED"
displaySystemErrorCode' ERROR_TM_INITIALIZATION_FAILED                                           = Just "ERROR_TM_INITIALIZATION_FAILED"
displaySystemErrorCode' ERROR_RESOURCEMANAGER_READ_ONLY                                          = Just "ERROR_RESOURCEMANAGER_READ_ONLY"
displaySystemErrorCode' ERROR_TRANSACTION_NOT_JOINED                                             = Just "ERROR_TRANSACTION_NOT_JOINED"
displaySystemErrorCode' ERROR_TRANSACTION_SUPERIOR_EXISTS                                        = Just "ERROR_TRANSACTION_SUPERIOR_EXISTS"
displaySystemErrorCode' ERROR_CRM_PROTOCOL_ALREADY_EXISTS                                        = Just "ERROR_CRM_PROTOCOL_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_TRANSACTION_PROPAGATION_FAILED                                     = Just "ERROR_TRANSACTION_PROPAGATION_FAILED"
displaySystemErrorCode' ERROR_CRM_PROTOCOL_NOT_FOUND                                             = Just "ERROR_CRM_PROTOCOL_NOT_FOUND"
displaySystemErrorCode' ERROR_TRANSACTION_INVALID_MARSHALL_BUFFER                                = Just "ERROR_TRANSACTION_INVALID_MARSHALL_BUFFER"
displaySystemErrorCode' ERROR_CURRENT_TRANSACTION_NOT_VALID                                      = Just "ERROR_CURRENT_TRANSACTION_NOT_VALID"
displaySystemErrorCode' ERROR_TRANSACTION_NOT_FOUND                                              = Just "ERROR_TRANSACTION_NOT_FOUND"
displaySystemErrorCode' ERROR_RESOURCEMANAGER_NOT_FOUND                                          = Just "ERROR_RESOURCEMANAGER_NOT_FOUND"
displaySystemErrorCode' ERROR_ENLISTMENT_NOT_FOUND                                               = Just "ERROR_ENLISTMENT_NOT_FOUND"
displaySystemErrorCode' ERROR_TRANSACTIONMANAGER_NOT_FOUND                                       = Just "ERROR_TRANSACTIONMANAGER_NOT_FOUND"
displaySystemErrorCode' ERROR_TRANSACTIONMANAGER_NOT_ONLINE                                      = Just "ERROR_TRANSACTIONMANAGER_NOT_ONLINE"
displaySystemErrorCode' ERROR_TRANSACTIONMANAGER_RECOVERY_NAME_COLLISION                         = Just "ERROR_TRANSACTIONMANAGER_RECOVERY_NAME_COLLISION"
displaySystemErrorCode' ERROR_TRANSACTION_NOT_ROOT                                               = Just "ERROR_TRANSACTION_NOT_ROOT"
displaySystemErrorCode' ERROR_TRANSACTION_OBJECT_EXPIRED                                         = Just "ERROR_TRANSACTION_OBJECT_EXPIRED"
displaySystemErrorCode' ERROR_TRANSACTION_RESPONSE_NOT_ENLISTED                                  = Just "ERROR_TRANSACTION_RESPONSE_NOT_ENLISTED"
displaySystemErrorCode' ERROR_TRANSACTION_RECORD_TOO_LONG                                        = Just "ERROR_TRANSACTION_RECORD_TOO_LONG"
displaySystemErrorCode' ERROR_IMPLICIT_TRANSACTION_NOT_SUPPORTED                                 = Just "ERROR_IMPLICIT_TRANSACTION_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_TRANSACTION_INTEGRITY_VIOLATED                                     = Just "ERROR_TRANSACTION_INTEGRITY_VIOLATED"
displaySystemErrorCode' ERROR_TRANSACTIONMANAGER_IDENTITY_MISMATCH                               = Just "ERROR_TRANSACTIONMANAGER_IDENTITY_MISMATCH"
displaySystemErrorCode' ERROR_RM_CANNOT_BE_FROZEN_FOR_SNAPSHOT                                   = Just "ERROR_RM_CANNOT_BE_FROZEN_FOR_SNAPSHOT"
displaySystemErrorCode' ERROR_TRANSACTION_MUST_WRITETHROUGH                                      = Just "ERROR_TRANSACTION_MUST_WRITETHROUGH"
displaySystemErrorCode' ERROR_TRANSACTION_NO_SUPERIOR                                            = Just "ERROR_TRANSACTION_NO_SUPERIOR"
displaySystemErrorCode' ERROR_HEURISTIC_DAMAGE_POSSIBLE                                          = Just "ERROR_HEURISTIC_DAMAGE_POSSIBLE"
displaySystemErrorCode' ERROR_TRANSACTIONAL_CONFLICT                                             = Just "ERROR_TRANSACTIONAL_CONFLICT"
displaySystemErrorCode' ERROR_RM_NOT_ACTIVE                                                      = Just "ERROR_RM_NOT_ACTIVE"
displaySystemErrorCode' ERROR_RM_METADATA_CORRUPT                                                = Just "ERROR_RM_METADATA_CORRUPT"
displaySystemErrorCode' ERROR_DIRECTORY_NOT_RM                                                   = Just "ERROR_DIRECTORY_NOT_RM"
displaySystemErrorCode' ERROR_TRANSACTIONS_UNSUPPORTED_REMOTE                                    = Just "ERROR_TRANSACTIONS_UNSUPPORTED_REMOTE"
displaySystemErrorCode' ERROR_LOG_RESIZE_INVALID_SIZE                                            = Just "ERROR_LOG_RESIZE_INVALID_SIZE"
displaySystemErrorCode' ERROR_OBJECT_NO_LONGER_EXISTS                                            = Just "ERROR_OBJECT_NO_LONGER_EXISTS"
displaySystemErrorCode' ERROR_STREAM_MINIVERSION_NOT_FOUND                                       = Just "ERROR_STREAM_MINIVERSION_NOT_FOUND"
displaySystemErrorCode' ERROR_STREAM_MINIVERSION_NOT_VALID                                       = Just "ERROR_STREAM_MINIVERSION_NOT_VALID"
displaySystemErrorCode' ERROR_MINIVERSION_INACCESSIBLE_FROM_SPECIFIED_TRANSACTION                = Just "ERROR_MINIVERSION_INACCESSIBLE_FROM_SPECIFIED_TRANSACTION"
displaySystemErrorCode' ERROR_CANT_OPEN_MINIVERSION_WITH_MODIFY_INTENT                           = Just "ERROR_CANT_OPEN_MINIVERSION_WITH_MODIFY_INTENT"
displaySystemErrorCode' ERROR_CANT_CREATE_MORE_STREAM_MINIVERSIONS                               = Just "ERROR_CANT_CREATE_MORE_STREAM_MINIVERSIONS"
displaySystemErrorCode' ERROR_REMOTE_FILE_VERSION_MISMATCH                                       = Just "ERROR_REMOTE_FILE_VERSION_MISMATCH"
displaySystemErrorCode' ERROR_HANDLE_NO_LONGER_VALID                                             = Just "ERROR_HANDLE_NO_LONGER_VALID"
displaySystemErrorCode' ERROR_NO_TXF_METADATA                                                    = Just "ERROR_NO_TXF_METADATA"
displaySystemErrorCode' ERROR_LOG_CORRUPTION_DETECTED                                            = Just "ERROR_LOG_CORRUPTION_DETECTED"
displaySystemErrorCode' ERROR_CANT_RECOVER_WITH_HANDLE_OPEN                                      = Just "ERROR_CANT_RECOVER_WITH_HANDLE_OPEN"
displaySystemErrorCode' ERROR_RM_DISCONNECTED                                                    = Just "ERROR_RM_DISCONNECTED"
displaySystemErrorCode' ERROR_ENLISTMENT_NOT_SUPERIOR                                            = Just "ERROR_ENLISTMENT_NOT_SUPERIOR"
displaySystemErrorCode' ERROR_RECOVERY_NOT_NEEDED                                                = Just "ERROR_RECOVERY_NOT_NEEDED"
displaySystemErrorCode' ERROR_RM_ALREADY_STARTED                                                 = Just "ERROR_RM_ALREADY_STARTED"
displaySystemErrorCode' ERROR_FILE_IDENTITY_NOT_PERSISTENT                                       = Just "ERROR_FILE_IDENTITY_NOT_PERSISTENT"
displaySystemErrorCode' ERROR_CANT_BREAK_TRANSACTIONAL_DEPENDENCY                                = Just "ERROR_CANT_BREAK_TRANSACTIONAL_DEPENDENCY"
displaySystemErrorCode' ERROR_CANT_CROSS_RM_BOUNDARY                                             = Just "ERROR_CANT_CROSS_RM_BOUNDARY"
displaySystemErrorCode' ERROR_TXF_DIR_NOT_EMPTY                                                  = Just "ERROR_TXF_DIR_NOT_EMPTY"
displaySystemErrorCode' ERROR_INDOUBT_TRANSACTIONS_EXIST                                         = Just "ERROR_INDOUBT_TRANSACTIONS_EXIST"
displaySystemErrorCode' ERROR_TM_VOLATILE                                                        = Just "ERROR_TM_VOLATILE"
displaySystemErrorCode' ERROR_ROLLBACK_TIMER_EXPIRED                                             = Just "ERROR_ROLLBACK_TIMER_EXPIRED"
displaySystemErrorCode' ERROR_TXF_ATTRIBUTE_CORRUPT                                              = Just "ERROR_TXF_ATTRIBUTE_CORRUPT"
displaySystemErrorCode' ERROR_EFS_NOT_ALLOWED_IN_TRANSACTION                                     = Just "ERROR_EFS_NOT_ALLOWED_IN_TRANSACTION"
displaySystemErrorCode' ERROR_TRANSACTIONAL_OPEN_NOT_ALLOWED                                     = Just "ERROR_TRANSACTIONAL_OPEN_NOT_ALLOWED"
displaySystemErrorCode' ERROR_LOG_GROWTH_FAILED                                                  = Just "ERROR_LOG_GROWTH_FAILED"
displaySystemErrorCode' ERROR_TRANSACTED_MAPPING_UNSUPPORTED_REMOTE                              = Just "ERROR_TRANSACTED_MAPPING_UNSUPPORTED_REMOTE"
displaySystemErrorCode' ERROR_TXF_METADATA_ALREADY_PRESENT                                       = Just "ERROR_TXF_METADATA_ALREADY_PRESENT"
displaySystemErrorCode' ERROR_TRANSACTION_SCOPE_CALLBACKS_NOT_SET                                = Just "ERROR_TRANSACTION_SCOPE_CALLBACKS_NOT_SET"
displaySystemErrorCode' ERROR_TRANSACTION_REQUIRED_PROMOTION                                     = Just "ERROR_TRANSACTION_REQUIRED_PROMOTION"
displaySystemErrorCode' ERROR_CANNOT_EXECUTE_FILE_IN_TRANSACTION                                 = Just "ERROR_CANNOT_EXECUTE_FILE_IN_TRANSACTION"
displaySystemErrorCode' ERROR_TRANSACTIONS_NOT_FROZEN                                            = Just "ERROR_TRANSACTIONS_NOT_FROZEN"
displaySystemErrorCode' ERROR_TRANSACTION_FREEZE_IN_PROGRESS                                     = Just "ERROR_TRANSACTION_FREEZE_IN_PROGRESS"
displaySystemErrorCode' ERROR_NOT_SNAPSHOT_VOLUME                                                = Just "ERROR_NOT_SNAPSHOT_VOLUME"
displaySystemErrorCode' ERROR_NO_SAVEPOINT_WITH_OPEN_FILES                                       = Just "ERROR_NO_SAVEPOINT_WITH_OPEN_FILES"
displaySystemErrorCode' ERROR_DATA_LOST_REPAIR                                                   = Just "ERROR_DATA_LOST_REPAIR"
displaySystemErrorCode' ERROR_SPARSE_NOT_ALLOWED_IN_TRANSACTION                                  = Just "ERROR_SPARSE_NOT_ALLOWED_IN_TRANSACTION"
displaySystemErrorCode' ERROR_TM_IDENTITY_MISMATCH                                               = Just "ERROR_TM_IDENTITY_MISMATCH"
displaySystemErrorCode' ERROR_FLOATED_SECTION                                                    = Just "ERROR_FLOATED_SECTION"
displaySystemErrorCode' ERROR_CANNOT_ACCEPT_TRANSACTED_WORK                                      = Just "ERROR_CANNOT_ACCEPT_TRANSACTED_WORK"
displaySystemErrorCode' ERROR_CANNOT_ABORT_TRANSACTIONS                                          = Just "ERROR_CANNOT_ABORT_TRANSACTIONS"
displaySystemErrorCode' ERROR_BAD_CLUSTERS                                                       = Just "ERROR_BAD_CLUSTERS"
displaySystemErrorCode' ERROR_COMPRESSION_NOT_ALLOWED_IN_TRANSACTION                             = Just "ERROR_COMPRESSION_NOT_ALLOWED_IN_TRANSACTION"
displaySystemErrorCode' ERROR_VOLUME_DIRTY                                                       = Just "ERROR_VOLUME_DIRTY"
displaySystemErrorCode' ERROR_NO_LINK_TRACKING_IN_TRANSACTION                                    = Just "ERROR_NO_LINK_TRACKING_IN_TRANSACTION"
displaySystemErrorCode' ERROR_OPERATION_NOT_SUPPORTED_IN_TRANSACTION                             = Just "ERROR_OPERATION_NOT_SUPPORTED_IN_TRANSACTION"
displaySystemErrorCode' ERROR_EXPIRED_HANDLE                                                     = Just "ERROR_EXPIRED_HANDLE"
displaySystemErrorCode' ERROR_TRANSACTION_NOT_ENLISTED                                           = Just "ERROR_TRANSACTION_NOT_ENLISTED"
displaySystemErrorCode' ERROR_CTX_WINSTATION_NAME_INVALID                                        = Just "ERROR_CTX_WINSTATION_NAME_INVALID"
displaySystemErrorCode' ERROR_CTX_INVALID_PD                                                     = Just "ERROR_CTX_INVALID_PD"
displaySystemErrorCode' ERROR_CTX_PD_NOT_FOUND                                                   = Just "ERROR_CTX_PD_NOT_FOUND"
displaySystemErrorCode' ERROR_CTX_WD_NOT_FOUND                                                   = Just "ERROR_CTX_WD_NOT_FOUND"
displaySystemErrorCode' ERROR_CTX_CANNOT_MAKE_EVENTLOG_ENTRY                                     = Just "ERROR_CTX_CANNOT_MAKE_EVENTLOG_ENTRY"
displaySystemErrorCode' ERROR_CTX_SERVICE_NAME_COLLISION                                         = Just "ERROR_CTX_SERVICE_NAME_COLLISION"
displaySystemErrorCode' ERROR_CTX_CLOSE_PENDING                                                  = Just "ERROR_CTX_CLOSE_PENDING"
displaySystemErrorCode' ERROR_CTX_NO_OUTBUF                                                      = Just "ERROR_CTX_NO_OUTBUF"
displaySystemErrorCode' ERROR_CTX_MODEM_INF_NOT_FOUND                                            = Just "ERROR_CTX_MODEM_INF_NOT_FOUND"
displaySystemErrorCode' ERROR_CTX_INVALID_MODEMNAME                                              = Just "ERROR_CTX_INVALID_MODEMNAME"
displaySystemErrorCode' ERROR_CTX_MODEM_RESPONSE_ERROR                                           = Just "ERROR_CTX_MODEM_RESPONSE_ERROR"
displaySystemErrorCode' ERROR_CTX_MODEM_RESPONSE_TIMEOUT                                         = Just "ERROR_CTX_MODEM_RESPONSE_TIMEOUT"
displaySystemErrorCode' ERROR_CTX_MODEM_RESPONSE_NO_CARRIER                                      = Just "ERROR_CTX_MODEM_RESPONSE_NO_CARRIER"
displaySystemErrorCode' ERROR_CTX_MODEM_RESPONSE_NO_DIALTONE                                     = Just "ERROR_CTX_MODEM_RESPONSE_NO_DIALTONE"
displaySystemErrorCode' ERROR_CTX_MODEM_RESPONSE_BUSY                                            = Just "ERROR_CTX_MODEM_RESPONSE_BUSY"
displaySystemErrorCode' ERROR_CTX_MODEM_RESPONSE_VOICE                                           = Just "ERROR_CTX_MODEM_RESPONSE_VOICE"
displaySystemErrorCode' ERROR_CTX_TD_ERROR                                                       = Just "ERROR_CTX_TD_ERROR"
displaySystemErrorCode' ERROR_CTX_WINSTATION_NOT_FOUND                                           = Just "ERROR_CTX_WINSTATION_NOT_FOUND"
displaySystemErrorCode' ERROR_CTX_WINSTATION_ALREADY_EXISTS                                      = Just "ERROR_CTX_WINSTATION_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_CTX_WINSTATION_BUSY                                                = Just "ERROR_CTX_WINSTATION_BUSY"
displaySystemErrorCode' ERROR_CTX_BAD_VIDEO_MODE                                                 = Just "ERROR_CTX_BAD_VIDEO_MODE"
displaySystemErrorCode' ERROR_CTX_GRAPHICS_INVALID                                               = Just "ERROR_CTX_GRAPHICS_INVALID"
displaySystemErrorCode' ERROR_CTX_LOGON_DISABLED                                                 = Just "ERROR_CTX_LOGON_DISABLED"
displaySystemErrorCode' ERROR_CTX_NOT_CONSOLE                                                    = Just "ERROR_CTX_NOT_CONSOLE"
displaySystemErrorCode' ERROR_CTX_CLIENT_QUERY_TIMEOUT                                           = Just "ERROR_CTX_CLIENT_QUERY_TIMEOUT"
displaySystemErrorCode' ERROR_CTX_CONSOLE_DISCONNECT                                             = Just "ERROR_CTX_CONSOLE_DISCONNECT"
displaySystemErrorCode' ERROR_CTX_CONSOLE_CONNECT                                                = Just "ERROR_CTX_CONSOLE_CONNECT"
displaySystemErrorCode' ERROR_CTX_SHADOW_DENIED                                                  = Just "ERROR_CTX_SHADOW_DENIED"
displaySystemErrorCode' ERROR_CTX_WINSTATION_ACCESS_DENIED                                       = Just "ERROR_CTX_WINSTATION_ACCESS_DENIED"
displaySystemErrorCode' ERROR_CTX_INVALID_WD                                                     = Just "ERROR_CTX_INVALID_WD"
displaySystemErrorCode' ERROR_CTX_SHADOW_INVALID                                                 = Just "ERROR_CTX_SHADOW_INVALID"
displaySystemErrorCode' ERROR_CTX_SHADOW_DISABLED                                                = Just "ERROR_CTX_SHADOW_DISABLED"
displaySystemErrorCode' ERROR_CTX_CLIENT_LICENSE_IN_USE                                          = Just "ERROR_CTX_CLIENT_LICENSE_IN_USE"
displaySystemErrorCode' ERROR_CTX_CLIENT_LICENSE_NOT_SET                                         = Just "ERROR_CTX_CLIENT_LICENSE_NOT_SET"
displaySystemErrorCode' ERROR_CTX_LICENSE_NOT_AVAILABLE                                          = Just "ERROR_CTX_LICENSE_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_CTX_LICENSE_CLIENT_INVALID                                         = Just "ERROR_CTX_LICENSE_CLIENT_INVALID"
displaySystemErrorCode' ERROR_CTX_LICENSE_EXPIRED                                                = Just "ERROR_CTX_LICENSE_EXPIRED"
displaySystemErrorCode' ERROR_CTX_SHADOW_NOT_RUNNING                                             = Just "ERROR_CTX_SHADOW_NOT_RUNNING"
displaySystemErrorCode' ERROR_CTX_SHADOW_ENDED_BY_MODE_CHANGE                                    = Just "ERROR_CTX_SHADOW_ENDED_BY_MODE_CHANGE"
displaySystemErrorCode' ERROR_ACTIVATION_COUNT_EXCEEDED                                          = Just "ERROR_ACTIVATION_COUNT_EXCEEDED"
displaySystemErrorCode' ERROR_CTX_WINSTATIONS_DISABLED                                           = Just "ERROR_CTX_WINSTATIONS_DISABLED"
displaySystemErrorCode' ERROR_CTX_ENCRYPTION_LEVEL_REQUIRED                                      = Just "ERROR_CTX_ENCRYPTION_LEVEL_REQUIRED"
displaySystemErrorCode' ERROR_CTX_SESSION_IN_USE                                                 = Just "ERROR_CTX_SESSION_IN_USE"
displaySystemErrorCode' ERROR_CTX_NO_FORCE_LOGOFF                                                = Just "ERROR_CTX_NO_FORCE_LOGOFF"
displaySystemErrorCode' ERROR_CTX_ACCOUNT_RESTRICTION                                            = Just "ERROR_CTX_ACCOUNT_RESTRICTION"
displaySystemErrorCode' ERROR_RDP_PROTOCOL_ERROR                                                 = Just "ERROR_RDP_PROTOCOL_ERROR"
displaySystemErrorCode' ERROR_CTX_CDM_CONNECT                                                    = Just "ERROR_CTX_CDM_CONNECT"
displaySystemErrorCode' ERROR_CTX_CDM_DISCONNECT                                                 = Just "ERROR_CTX_CDM_DISCONNECT"
displaySystemErrorCode' ERROR_CTX_SECURITY_LAYER_ERROR                                           = Just "ERROR_CTX_SECURITY_LAYER_ERROR"
displaySystemErrorCode' ERROR_TS_INCOMPATIBLE_SESSIONS                                           = Just "ERROR_TS_INCOMPATIBLE_SESSIONS"
displaySystemErrorCode' ERROR_TS_VIDEO_SUBSYSTEM_ERROR                                           = Just "ERROR_TS_VIDEO_SUBSYSTEM_ERROR"
displaySystemErrorCode' ERROR_DS_NOT_INSTALLED                                                   = Just "ERROR_DS_NOT_INSTALLED"
displaySystemErrorCode' ERROR_DS_MEMBERSHIP_EVALUATED_LOCALLY                                    = Just "ERROR_DS_MEMBERSHIP_EVALUATED_LOCALLY"
displaySystemErrorCode' ERROR_DS_NO_ATTRIBUTE_OR_VALUE                                           = Just "ERROR_DS_NO_ATTRIBUTE_OR_VALUE"
displaySystemErrorCode' ERROR_DS_INVALID_ATTRIBUTE_SYNTAX                                        = Just "ERROR_DS_INVALID_ATTRIBUTE_SYNTAX"
displaySystemErrorCode' ERROR_DS_ATTRIBUTE_TYPE_UNDEFINED                                        = Just "ERROR_DS_ATTRIBUTE_TYPE_UNDEFINED"
displaySystemErrorCode' ERROR_DS_ATTRIBUTE_OR_VALUE_EXISTS                                       = Just "ERROR_DS_ATTRIBUTE_OR_VALUE_EXISTS"
displaySystemErrorCode' ERROR_DS_BUSY                                                            = Just "ERROR_DS_BUSY"
displaySystemErrorCode' ERROR_DS_UNAVAILABLE                                                     = Just "ERROR_DS_UNAVAILABLE"
displaySystemErrorCode' ERROR_DS_NO_RIDS_ALLOCATED                                               = Just "ERROR_DS_NO_RIDS_ALLOCATED"
displaySystemErrorCode' ERROR_DS_NO_MORE_RIDS                                                    = Just "ERROR_DS_NO_MORE_RIDS"
displaySystemErrorCode' ERROR_DS_INCORRECT_ROLE_OWNER                                            = Just "ERROR_DS_INCORRECT_ROLE_OWNER"
displaySystemErrorCode' ERROR_DS_RIDMGR_INIT_ERROR                                               = Just "ERROR_DS_RIDMGR_INIT_ERROR"
displaySystemErrorCode' ERROR_DS_OBJ_CLASS_VIOLATION                                             = Just "ERROR_DS_OBJ_CLASS_VIOLATION"
displaySystemErrorCode' ERROR_DS_CANT_ON_NON_LEAF                                                = Just "ERROR_DS_CANT_ON_NON_LEAF"
displaySystemErrorCode' ERROR_DS_CANT_ON_RDN                                                     = Just "ERROR_DS_CANT_ON_RDN"
displaySystemErrorCode' ERROR_DS_CANT_MOD_OBJ_CLASS                                              = Just "ERROR_DS_CANT_MOD_OBJ_CLASS"
displaySystemErrorCode' ERROR_DS_CROSS_DOM_MOVE_ERROR                                            = Just "ERROR_DS_CROSS_DOM_MOVE_ERROR"
displaySystemErrorCode' ERROR_DS_GC_NOT_AVAILABLE                                                = Just "ERROR_DS_GC_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_SHARED_POLICY                                                      = Just "ERROR_SHARED_POLICY"
displaySystemErrorCode' ERROR_POLICY_OBJECT_NOT_FOUND                                            = Just "ERROR_POLICY_OBJECT_NOT_FOUND"
displaySystemErrorCode' ERROR_POLICY_ONLY_IN_DS                                                  = Just "ERROR_POLICY_ONLY_IN_DS"
displaySystemErrorCode' ERROR_PROMOTION_ACTIVE                                                   = Just "ERROR_PROMOTION_ACTIVE"
displaySystemErrorCode' ERROR_NO_PROMOTION_ACTIVE                                                = Just "ERROR_NO_PROMOTION_ACTIVE"
displaySystemErrorCode' ERROR_DS_OPERATIONS_ERROR                                                = Just "ERROR_DS_OPERATIONS_ERROR"
displaySystemErrorCode' ERROR_DS_PROTOCOL_ERROR                                                  = Just "ERROR_DS_PROTOCOL_ERROR"
displaySystemErrorCode' ERROR_DS_TIMELIMIT_EXCEEDED                                              = Just "ERROR_DS_TIMELIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_DS_SIZELIMIT_EXCEEDED                                              = Just "ERROR_DS_SIZELIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_DS_ADMIN_LIMIT_EXCEEDED                                            = Just "ERROR_DS_ADMIN_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_DS_COMPARE_FALSE                                                   = Just "ERROR_DS_COMPARE_FALSE"
displaySystemErrorCode' ERROR_DS_COMPARE_TRUE                                                    = Just "ERROR_DS_COMPARE_TRUE"
displaySystemErrorCode' ERROR_DS_AUTH_METHOD_NOT_SUPPORTED                                       = Just "ERROR_DS_AUTH_METHOD_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_DS_STRONG_AUTH_REQUIRED                                            = Just "ERROR_DS_STRONG_AUTH_REQUIRED"
displaySystemErrorCode' ERROR_DS_INAPPROPRIATE_AUTH                                              = Just "ERROR_DS_INAPPROPRIATE_AUTH"
displaySystemErrorCode' ERROR_DS_AUTH_UNKNOWN                                                    = Just "ERROR_DS_AUTH_UNKNOWN"
displaySystemErrorCode' ERROR_DS_REFERRAL                                                        = Just "ERROR_DS_REFERRAL"
displaySystemErrorCode' ERROR_DS_UNAVAILABLE_CRIT_EXTENSION                                      = Just "ERROR_DS_UNAVAILABLE_CRIT_EXTENSION"
displaySystemErrorCode' ERROR_DS_CONFIDENTIALITY_REQUIRED                                        = Just "ERROR_DS_CONFIDENTIALITY_REQUIRED"
displaySystemErrorCode' ERROR_DS_INAPPROPRIATE_MATCHING                                          = Just "ERROR_DS_INAPPROPRIATE_MATCHING"
displaySystemErrorCode' ERROR_DS_CONSTRAINT_VIOLATION                                            = Just "ERROR_DS_CONSTRAINT_VIOLATION"
displaySystemErrorCode' ERROR_DS_NO_SUCH_OBJECT                                                  = Just "ERROR_DS_NO_SUCH_OBJECT"
displaySystemErrorCode' ERROR_DS_ALIAS_PROBLEM                                                   = Just "ERROR_DS_ALIAS_PROBLEM"
displaySystemErrorCode' ERROR_DS_INVALID_DN_SYNTAX                                               = Just "ERROR_DS_INVALID_DN_SYNTAX"
displaySystemErrorCode' ERROR_DS_IS_LEAF                                                         = Just "ERROR_DS_IS_LEAF"
displaySystemErrorCode' ERROR_DS_ALIAS_DEREF_PROBLEM                                             = Just "ERROR_DS_ALIAS_DEREF_PROBLEM"
displaySystemErrorCode' ERROR_DS_UNWILLING_TO_PERFORM                                            = Just "ERROR_DS_UNWILLING_TO_PERFORM"
displaySystemErrorCode' ERROR_DS_LOOP_DETECT                                                     = Just "ERROR_DS_LOOP_DETECT"
displaySystemErrorCode' ERROR_DS_NAMING_VIOLATION                                                = Just "ERROR_DS_NAMING_VIOLATION"
displaySystemErrorCode' ERROR_DS_OBJECT_RESULTS_TOO_LARGE                                        = Just "ERROR_DS_OBJECT_RESULTS_TOO_LARGE"
displaySystemErrorCode' ERROR_DS_AFFECTS_MULTIPLE_DSAS                                           = Just "ERROR_DS_AFFECTS_MULTIPLE_DSAS"
displaySystemErrorCode' ERROR_DS_SERVER_DOWN                                                     = Just "ERROR_DS_SERVER_DOWN"
displaySystemErrorCode' ERROR_DS_LOCAL_ERROR                                                     = Just "ERROR_DS_LOCAL_ERROR"
displaySystemErrorCode' ERROR_DS_ENCODING_ERROR                                                  = Just "ERROR_DS_ENCODING_ERROR"
displaySystemErrorCode' ERROR_DS_DECODING_ERROR                                                  = Just "ERROR_DS_DECODING_ERROR"
displaySystemErrorCode' ERROR_DS_FILTER_UNKNOWN                                                  = Just "ERROR_DS_FILTER_UNKNOWN"
displaySystemErrorCode' ERROR_DS_PARAM_ERROR                                                     = Just "ERROR_DS_PARAM_ERROR"
displaySystemErrorCode' ERROR_DS_NOT_SUPPORTED                                                   = Just "ERROR_DS_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_DS_NO_RESULTS_RETURNED                                             = Just "ERROR_DS_NO_RESULTS_RETURNED"
displaySystemErrorCode' ERROR_DS_CONTROL_NOT_FOUND                                               = Just "ERROR_DS_CONTROL_NOT_FOUND"
displaySystemErrorCode' ERROR_DS_CLIENT_LOOP                                                     = Just "ERROR_DS_CLIENT_LOOP"
displaySystemErrorCode' ERROR_DS_REFERRAL_LIMIT_EXCEEDED                                         = Just "ERROR_DS_REFERRAL_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_DS_SORT_CONTROL_MISSING                                            = Just "ERROR_DS_SORT_CONTROL_MISSING"
displaySystemErrorCode' ERROR_DS_OFFSET_RANGE_ERROR                                              = Just "ERROR_DS_OFFSET_RANGE_ERROR"
displaySystemErrorCode' ERROR_DS_RIDMGR_DISABLED                                                 = Just "ERROR_DS_RIDMGR_DISABLED"
displaySystemErrorCode' ERROR_DS_ROOT_MUST_BE_NC                                                 = Just "ERROR_DS_ROOT_MUST_BE_NC"
displaySystemErrorCode' ERROR_DS_ADD_REPLICA_INHIBITED                                           = Just "ERROR_DS_ADD_REPLICA_INHIBITED"
displaySystemErrorCode' ERROR_DS_ATT_NOT_DEF_IN_SCHEMA                                           = Just "ERROR_DS_ATT_NOT_DEF_IN_SCHEMA"
displaySystemErrorCode' ERROR_DS_MAX_OBJ_SIZE_EXCEEDED                                           = Just "ERROR_DS_MAX_OBJ_SIZE_EXCEEDED"
displaySystemErrorCode' ERROR_DS_OBJ_STRING_NAME_EXISTS                                          = Just "ERROR_DS_OBJ_STRING_NAME_EXISTS"
displaySystemErrorCode' ERROR_DS_NO_RDN_DEFINED_IN_SCHEMA                                        = Just "ERROR_DS_NO_RDN_DEFINED_IN_SCHEMA"
displaySystemErrorCode' ERROR_DS_RDN_DOESNT_MATCH_SCHEMA                                         = Just "ERROR_DS_RDN_DOESNT_MATCH_SCHEMA"
displaySystemErrorCode' ERROR_DS_NO_REQUESTED_ATTS_FOUND                                         = Just "ERROR_DS_NO_REQUESTED_ATTS_FOUND"
displaySystemErrorCode' ERROR_DS_USER_BUFFER_TO_SMALL                                            = Just "ERROR_DS_USER_BUFFER_TO_SMALL"
displaySystemErrorCode' ERROR_DS_ATT_IS_NOT_ON_OBJ                                               = Just "ERROR_DS_ATT_IS_NOT_ON_OBJ"
displaySystemErrorCode' ERROR_DS_ILLEGAL_MOD_OPERATION                                           = Just "ERROR_DS_ILLEGAL_MOD_OPERATION"
displaySystemErrorCode' ERROR_DS_OBJ_TOO_LARGE                                                   = Just "ERROR_DS_OBJ_TOO_LARGE"
displaySystemErrorCode' ERROR_DS_BAD_INSTANCE_TYPE                                               = Just "ERROR_DS_BAD_INSTANCE_TYPE"
displaySystemErrorCode' ERROR_DS_MASTERDSA_REQUIRED                                              = Just "ERROR_DS_MASTERDSA_REQUIRED"
displaySystemErrorCode' ERROR_DS_OBJECT_CLASS_REQUIRED                                           = Just "ERROR_DS_OBJECT_CLASS_REQUIRED"
displaySystemErrorCode' ERROR_DS_MISSING_REQUIRED_ATT                                            = Just "ERROR_DS_MISSING_REQUIRED_ATT"
displaySystemErrorCode' ERROR_DS_ATT_NOT_DEF_FOR_CLASS                                           = Just "ERROR_DS_ATT_NOT_DEF_FOR_CLASS"
displaySystemErrorCode' ERROR_DS_ATT_ALREADY_EXISTS                                              = Just "ERROR_DS_ATT_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_DS_CANT_ADD_ATT_VALUES                                             = Just "ERROR_DS_CANT_ADD_ATT_VALUES"
displaySystemErrorCode' ERROR_DS_SINGLE_VALUE_CONSTRAINT                                         = Just "ERROR_DS_SINGLE_VALUE_CONSTRAINT"
displaySystemErrorCode' ERROR_DS_RANGE_CONSTRAINT                                                = Just "ERROR_DS_RANGE_CONSTRAINT"
displaySystemErrorCode' ERROR_DS_ATT_VAL_ALREADY_EXISTS                                          = Just "ERROR_DS_ATT_VAL_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_DS_CANT_REM_MISSING_ATT                                            = Just "ERROR_DS_CANT_REM_MISSING_ATT"
displaySystemErrorCode' ERROR_DS_CANT_REM_MISSING_ATT_VAL                                        = Just "ERROR_DS_CANT_REM_MISSING_ATT_VAL"
displaySystemErrorCode' ERROR_DS_ROOT_CANT_BE_SUBREF                                             = Just "ERROR_DS_ROOT_CANT_BE_SUBREF"
displaySystemErrorCode' ERROR_DS_NO_CHAINING                                                     = Just "ERROR_DS_NO_CHAINING"
displaySystemErrorCode' ERROR_DS_NO_CHAINED_EVAL                                                 = Just "ERROR_DS_NO_CHAINED_EVAL"
displaySystemErrorCode' ERROR_DS_NO_PARENT_OBJECT                                                = Just "ERROR_DS_NO_PARENT_OBJECT"
displaySystemErrorCode' ERROR_DS_PARENT_IS_AN_ALIAS                                              = Just "ERROR_DS_PARENT_IS_AN_ALIAS"
displaySystemErrorCode' ERROR_DS_CANT_MIX_MASTER_AND_REPS                                        = Just "ERROR_DS_CANT_MIX_MASTER_AND_REPS"
displaySystemErrorCode' ERROR_DS_CHILDREN_EXIST                                                  = Just "ERROR_DS_CHILDREN_EXIST"
displaySystemErrorCode' ERROR_DS_OBJ_NOT_FOUND                                                   = Just "ERROR_DS_OBJ_NOT_FOUND"
displaySystemErrorCode' ERROR_DS_ALIASED_OBJ_MISSING                                             = Just "ERROR_DS_ALIASED_OBJ_MISSING"
displaySystemErrorCode' ERROR_DS_BAD_NAME_SYNTAX                                                 = Just "ERROR_DS_BAD_NAME_SYNTAX"
displaySystemErrorCode' ERROR_DS_ALIAS_POINTS_TO_ALIAS                                           = Just "ERROR_DS_ALIAS_POINTS_TO_ALIAS"
displaySystemErrorCode' ERROR_DS_CANT_DEREF_ALIAS                                                = Just "ERROR_DS_CANT_DEREF_ALIAS"
displaySystemErrorCode' ERROR_DS_OUT_OF_SCOPE                                                    = Just "ERROR_DS_OUT_OF_SCOPE"
displaySystemErrorCode' ERROR_DS_OBJECT_BEING_REMOVED                                            = Just "ERROR_DS_OBJECT_BEING_REMOVED"
displaySystemErrorCode' ERROR_DS_CANT_DELETE_DSA_OBJ                                             = Just "ERROR_DS_CANT_DELETE_DSA_OBJ"
displaySystemErrorCode' ERROR_DS_GENERIC_ERROR                                                   = Just "ERROR_DS_GENERIC_ERROR"
displaySystemErrorCode' ERROR_DS_DSA_MUST_BE_INT_MASTER                                          = Just "ERROR_DS_DSA_MUST_BE_INT_MASTER"
displaySystemErrorCode' ERROR_DS_CLASS_NOT_DSA                                                   = Just "ERROR_DS_CLASS_NOT_DSA"
displaySystemErrorCode' ERROR_DS_INSUFF_ACCESS_RIGHTS                                            = Just "ERROR_DS_INSUFF_ACCESS_RIGHTS"
displaySystemErrorCode' ERROR_DS_ILLEGAL_SUPERIOR                                                = Just "ERROR_DS_ILLEGAL_SUPERIOR"
displaySystemErrorCode' ERROR_DS_ATTRIBUTE_OWNED_BY_SAM                                          = Just "ERROR_DS_ATTRIBUTE_OWNED_BY_SAM"
displaySystemErrorCode' ERROR_DS_NAME_TOO_MANY_PARTS                                             = Just "ERROR_DS_NAME_TOO_MANY_PARTS"
displaySystemErrorCode' ERROR_DS_NAME_TOO_LONG                                                   = Just "ERROR_DS_NAME_TOO_LONG"
displaySystemErrorCode' ERROR_DS_NAME_VALUE_TOO_LONG                                             = Just "ERROR_DS_NAME_VALUE_TOO_LONG"
displaySystemErrorCode' ERROR_DS_NAME_UNPARSEABLE                                                = Just "ERROR_DS_NAME_UNPARSEABLE"
displaySystemErrorCode' ERROR_DS_NAME_TYPE_UNKNOWN                                               = Just "ERROR_DS_NAME_TYPE_UNKNOWN"
displaySystemErrorCode' ERROR_DS_NOT_AN_OBJECT                                                   = Just "ERROR_DS_NOT_AN_OBJECT"
displaySystemErrorCode' ERROR_DS_SEC_DESC_TOO_SHORT                                              = Just "ERROR_DS_SEC_DESC_TOO_SHORT"
displaySystemErrorCode' ERROR_DS_SEC_DESC_INVALID                                                = Just "ERROR_DS_SEC_DESC_INVALID"
displaySystemErrorCode' ERROR_DS_NO_DELETED_NAME                                                 = Just "ERROR_DS_NO_DELETED_NAME"
displaySystemErrorCode' ERROR_DS_SUBREF_MUST_HAVE_PARENT                                         = Just "ERROR_DS_SUBREF_MUST_HAVE_PARENT"
displaySystemErrorCode' ERROR_DS_NCNAME_MUST_BE_NC                                               = Just "ERROR_DS_NCNAME_MUST_BE_NC"
displaySystemErrorCode' ERROR_DS_CANT_ADD_SYSTEM_ONLY                                            = Just "ERROR_DS_CANT_ADD_SYSTEM_ONLY"
displaySystemErrorCode' ERROR_DS_CLASS_MUST_BE_CONCRETE                                          = Just "ERROR_DS_CLASS_MUST_BE_CONCRETE"
displaySystemErrorCode' ERROR_DS_INVALID_DMD                                                     = Just "ERROR_DS_INVALID_DMD"
displaySystemErrorCode' ERROR_DS_OBJ_GUID_EXISTS                                                 = Just "ERROR_DS_OBJ_GUID_EXISTS"
displaySystemErrorCode' ERROR_DS_NOT_ON_BACKLINK                                                 = Just "ERROR_DS_NOT_ON_BACKLINK"
displaySystemErrorCode' ERROR_DS_NO_CROSSREF_FOR_NC                                              = Just "ERROR_DS_NO_CROSSREF_FOR_NC"
displaySystemErrorCode' ERROR_DS_SHUTTING_DOWN                                                   = Just "ERROR_DS_SHUTTING_DOWN"
displaySystemErrorCode' ERROR_DS_UNKNOWN_OPERATION                                               = Just "ERROR_DS_UNKNOWN_OPERATION"
displaySystemErrorCode' ERROR_DS_INVALID_ROLE_OWNER                                              = Just "ERROR_DS_INVALID_ROLE_OWNER"
displaySystemErrorCode' ERROR_DS_COULDNT_CONTACT_FSMO                                            = Just "ERROR_DS_COULDNT_CONTACT_FSMO"
displaySystemErrorCode' ERROR_DS_CROSS_NC_DN_RENAME                                              = Just "ERROR_DS_CROSS_NC_DN_RENAME"
displaySystemErrorCode' ERROR_DS_CANT_MOD_SYSTEM_ONLY                                            = Just "ERROR_DS_CANT_MOD_SYSTEM_ONLY"
displaySystemErrorCode' ERROR_DS_REPLICATOR_ONLY                                                 = Just "ERROR_DS_REPLICATOR_ONLY"
displaySystemErrorCode' ERROR_DS_OBJ_CLASS_NOT_DEFINED                                           = Just "ERROR_DS_OBJ_CLASS_NOT_DEFINED"
displaySystemErrorCode' ERROR_DS_OBJ_CLASS_NOT_SUBCLASS                                          = Just "ERROR_DS_OBJ_CLASS_NOT_SUBCLASS"
displaySystemErrorCode' ERROR_DS_NAME_REFERENCE_INVALID                                          = Just "ERROR_DS_NAME_REFERENCE_INVALID"
displaySystemErrorCode' ERROR_DS_CROSS_REF_EXISTS                                                = Just "ERROR_DS_CROSS_REF_EXISTS"
displaySystemErrorCode' ERROR_DS_CANT_DEL_MASTER_CROSSREF                                        = Just "ERROR_DS_CANT_DEL_MASTER_CROSSREF"
displaySystemErrorCode' ERROR_DS_SUBTREE_NOTIFY_NOT_NC_HEAD                                      = Just "ERROR_DS_SUBTREE_NOTIFY_NOT_NC_HEAD"
displaySystemErrorCode' ERROR_DS_NOTIFY_FILTER_TOO_COMPLEX                                       = Just "ERROR_DS_NOTIFY_FILTER_TOO_COMPLEX"
displaySystemErrorCode' ERROR_DS_DUP_RDN                                                         = Just "ERROR_DS_DUP_RDN"
displaySystemErrorCode' ERROR_DS_DUP_OID                                                         = Just "ERROR_DS_DUP_OID"
displaySystemErrorCode' ERROR_DS_DUP_MAPI_ID                                                     = Just "ERROR_DS_DUP_MAPI_ID"
displaySystemErrorCode' ERROR_DS_DUP_SCHEMA_ID_GUID                                              = Just "ERROR_DS_DUP_SCHEMA_ID_GUID"
displaySystemErrorCode' ERROR_DS_DUP_LDAP_DISPLAY_NAME                                           = Just "ERROR_DS_DUP_LDAP_DISPLAY_NAME"
displaySystemErrorCode' ERROR_DS_SEMANTIC_ATT_TEST                                               = Just "ERROR_DS_SEMANTIC_ATT_TEST"
displaySystemErrorCode' ERROR_DS_SYNTAX_MISMATCH                                                 = Just "ERROR_DS_SYNTAX_MISMATCH"
displaySystemErrorCode' ERROR_DS_EXISTS_IN_MUST_HAVE                                             = Just "ERROR_DS_EXISTS_IN_MUST_HAVE"
displaySystemErrorCode' ERROR_DS_EXISTS_IN_MAY_HAVE                                              = Just "ERROR_DS_EXISTS_IN_MAY_HAVE"
displaySystemErrorCode' ERROR_DS_NONEXISTENT_MAY_HAVE                                            = Just "ERROR_DS_NONEXISTENT_MAY_HAVE"
displaySystemErrorCode' ERROR_DS_NONEXISTENT_MUST_HAVE                                           = Just "ERROR_DS_NONEXISTENT_MUST_HAVE"
displaySystemErrorCode' ERROR_DS_AUX_CLS_TEST_FAIL                                               = Just "ERROR_DS_AUX_CLS_TEST_FAIL"
displaySystemErrorCode' ERROR_DS_NONEXISTENT_POSS_SUP                                            = Just "ERROR_DS_NONEXISTENT_POSS_SUP"
displaySystemErrorCode' ERROR_DS_SUB_CLS_TEST_FAIL                                               = Just "ERROR_DS_SUB_CLS_TEST_FAIL"
displaySystemErrorCode' ERROR_DS_BAD_RDN_ATT_ID_SYNTAX                                           = Just "ERROR_DS_BAD_RDN_ATT_ID_SYNTAX"
displaySystemErrorCode' ERROR_DS_EXISTS_IN_AUX_CLS                                               = Just "ERROR_DS_EXISTS_IN_AUX_CLS"
displaySystemErrorCode' ERROR_DS_EXISTS_IN_SUB_CLS                                               = Just "ERROR_DS_EXISTS_IN_SUB_CLS"
displaySystemErrorCode' ERROR_DS_EXISTS_IN_POSS_SUP                                              = Just "ERROR_DS_EXISTS_IN_POSS_SUP"
displaySystemErrorCode' ERROR_DS_RECALCSCHEMA_FAILED                                             = Just "ERROR_DS_RECALCSCHEMA_FAILED"
displaySystemErrorCode' ERROR_DS_TREE_DELETE_NOT_FINISHED                                        = Just "ERROR_DS_TREE_DELETE_NOT_FINISHED"
displaySystemErrorCode' ERROR_DS_CANT_DELETE                                                     = Just "ERROR_DS_CANT_DELETE"
displaySystemErrorCode' ERROR_DS_ATT_SCHEMA_REQ_ID                                               = Just "ERROR_DS_ATT_SCHEMA_REQ_ID"
displaySystemErrorCode' ERROR_DS_BAD_ATT_SCHEMA_SYNTAX                                           = Just "ERROR_DS_BAD_ATT_SCHEMA_SYNTAX"
displaySystemErrorCode' ERROR_DS_CANT_CACHE_ATT                                                  = Just "ERROR_DS_CANT_CACHE_ATT"
displaySystemErrorCode' ERROR_DS_CANT_CACHE_CLASS                                                = Just "ERROR_DS_CANT_CACHE_CLASS"
displaySystemErrorCode' ERROR_DS_CANT_REMOVE_ATT_CACHE                                           = Just "ERROR_DS_CANT_REMOVE_ATT_CACHE"
displaySystemErrorCode' ERROR_DS_CANT_REMOVE_CLASS_CACHE                                         = Just "ERROR_DS_CANT_REMOVE_CLASS_CACHE"
displaySystemErrorCode' ERROR_DS_CANT_RETRIEVE_DN                                                = Just "ERROR_DS_CANT_RETRIEVE_DN"
displaySystemErrorCode' ERROR_DS_MISSING_SUPREF                                                  = Just "ERROR_DS_MISSING_SUPREF"
displaySystemErrorCode' ERROR_DS_CANT_RETRIEVE_INSTANCE                                          = Just "ERROR_DS_CANT_RETRIEVE_INSTANCE"
displaySystemErrorCode' ERROR_DS_CODE_INCONSISTENCY                                              = Just "ERROR_DS_CODE_INCONSISTENCY"
displaySystemErrorCode' ERROR_DS_DATABASE_ERROR                                                  = Just "ERROR_DS_DATABASE_ERROR"
displaySystemErrorCode' ERROR_DS_GOVERNSID_MISSING                                               = Just "ERROR_DS_GOVERNSID_MISSING"
displaySystemErrorCode' ERROR_DS_MISSING_EXPECTED_ATT                                            = Just "ERROR_DS_MISSING_EXPECTED_ATT"
displaySystemErrorCode' ERROR_DS_NCNAME_MISSING_CR_REF                                           = Just "ERROR_DS_NCNAME_MISSING_CR_REF"
displaySystemErrorCode' ERROR_DS_SECURITY_CHECKING_ERROR                                         = Just "ERROR_DS_SECURITY_CHECKING_ERROR"
displaySystemErrorCode' ERROR_DS_SCHEMA_NOT_LOADED                                               = Just "ERROR_DS_SCHEMA_NOT_LOADED"
displaySystemErrorCode' ERROR_DS_SCHEMA_ALLOC_FAILED                                             = Just "ERROR_DS_SCHEMA_ALLOC_FAILED"
displaySystemErrorCode' ERROR_DS_ATT_SCHEMA_REQ_SYNTAX                                           = Just "ERROR_DS_ATT_SCHEMA_REQ_SYNTAX"
displaySystemErrorCode' ERROR_DS_GCVERIFY_ERROR                                                  = Just "ERROR_DS_GCVERIFY_ERROR"
displaySystemErrorCode' ERROR_DS_DRA_SCHEMA_MISMATCH                                             = Just "ERROR_DS_DRA_SCHEMA_MISMATCH"
displaySystemErrorCode' ERROR_DS_CANT_FIND_DSA_OBJ                                               = Just "ERROR_DS_CANT_FIND_DSA_OBJ"
displaySystemErrorCode' ERROR_DS_CANT_FIND_EXPECTED_NC                                           = Just "ERROR_DS_CANT_FIND_EXPECTED_NC"
displaySystemErrorCode' ERROR_DS_CANT_FIND_NC_IN_CACHE                                           = Just "ERROR_DS_CANT_FIND_NC_IN_CACHE"
displaySystemErrorCode' ERROR_DS_CANT_RETRIEVE_CHILD                                             = Just "ERROR_DS_CANT_RETRIEVE_CHILD"
displaySystemErrorCode' ERROR_DS_SECURITY_ILLEGAL_MODIFY                                         = Just "ERROR_DS_SECURITY_ILLEGAL_MODIFY"
displaySystemErrorCode' ERROR_DS_CANT_REPLACE_HIDDEN_REC                                         = Just "ERROR_DS_CANT_REPLACE_HIDDEN_REC"
displaySystemErrorCode' ERROR_DS_BAD_HIERARCHY_FILE                                              = Just "ERROR_DS_BAD_HIERARCHY_FILE"
displaySystemErrorCode' ERROR_DS_BUILD_HIERARCHY_TABLE_FAILED                                    = Just "ERROR_DS_BUILD_HIERARCHY_TABLE_FAILED"
displaySystemErrorCode' ERROR_DS_CONFIG_PARAM_MISSING                                            = Just "ERROR_DS_CONFIG_PARAM_MISSING"
displaySystemErrorCode' ERROR_DS_COUNTING_AB_INDICES_FAILED                                      = Just "ERROR_DS_COUNTING_AB_INDICES_FAILED"
displaySystemErrorCode' ERROR_DS_HIERARCHY_TABLE_MALLOC_FAILED                                   = Just "ERROR_DS_HIERARCHY_TABLE_MALLOC_FAILED"
displaySystemErrorCode' ERROR_DS_INTERNAL_FAILURE                                                = Just "ERROR_DS_INTERNAL_FAILURE"
displaySystemErrorCode' ERROR_DS_UNKNOWN_ERROR                                                   = Just "ERROR_DS_UNKNOWN_ERROR"
displaySystemErrorCode' ERROR_DS_ROOT_REQUIRES_CLASS_TOP                                         = Just "ERROR_DS_ROOT_REQUIRES_CLASS_TOP"
displaySystemErrorCode' ERROR_DS_REFUSING_FSMO_ROLES                                             = Just "ERROR_DS_REFUSING_FSMO_ROLES"
displaySystemErrorCode' ERROR_DS_MISSING_FSMO_SETTINGS                                           = Just "ERROR_DS_MISSING_FSMO_SETTINGS"
displaySystemErrorCode' ERROR_DS_UNABLE_TO_SURRENDER_ROLES                                       = Just "ERROR_DS_UNABLE_TO_SURRENDER_ROLES"
displaySystemErrorCode' ERROR_DS_DRA_GENERIC                                                     = Just "ERROR_DS_DRA_GENERIC"
displaySystemErrorCode' ERROR_DS_DRA_INVALID_PARAMETER                                           = Just "ERROR_DS_DRA_INVALID_PARAMETER"
displaySystemErrorCode' ERROR_DS_DRA_BUSY                                                        = Just "ERROR_DS_DRA_BUSY"
displaySystemErrorCode' ERROR_DS_DRA_BAD_DN                                                      = Just "ERROR_DS_DRA_BAD_DN"
displaySystemErrorCode' ERROR_DS_DRA_BAD_NC                                                      = Just "ERROR_DS_DRA_BAD_NC"
displaySystemErrorCode' ERROR_DS_DRA_DN_EXISTS                                                   = Just "ERROR_DS_DRA_DN_EXISTS"
displaySystemErrorCode' ERROR_DS_DRA_INTERNAL_ERROR                                              = Just "ERROR_DS_DRA_INTERNAL_ERROR"
displaySystemErrorCode' ERROR_DS_DRA_INCONSISTENT_DIT                                            = Just "ERROR_DS_DRA_INCONSISTENT_DIT"
displaySystemErrorCode' ERROR_DS_DRA_CONNECTION_FAILED                                           = Just "ERROR_DS_DRA_CONNECTION_FAILED"
displaySystemErrorCode' ERROR_DS_DRA_BAD_INSTANCE_TYPE                                           = Just "ERROR_DS_DRA_BAD_INSTANCE_TYPE"
displaySystemErrorCode' ERROR_DS_DRA_OUT_OF_MEM                                                  = Just "ERROR_DS_DRA_OUT_OF_MEM"
displaySystemErrorCode' ERROR_DS_DRA_MAIL_PROBLEM                                                = Just "ERROR_DS_DRA_MAIL_PROBLEM"
displaySystemErrorCode' ERROR_DS_DRA_REF_ALREADY_EXISTS                                          = Just "ERROR_DS_DRA_REF_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_DS_DRA_REF_NOT_FOUND                                               = Just "ERROR_DS_DRA_REF_NOT_FOUND"
displaySystemErrorCode' ERROR_DS_DRA_OBJ_IS_REP_SOURCE                                           = Just "ERROR_DS_DRA_OBJ_IS_REP_SOURCE"
displaySystemErrorCode' ERROR_DS_DRA_DB_ERROR                                                    = Just "ERROR_DS_DRA_DB_ERROR"
displaySystemErrorCode' ERROR_DS_DRA_NO_REPLICA                                                  = Just "ERROR_DS_DRA_NO_REPLICA"
displaySystemErrorCode' ERROR_DS_DRA_ACCESS_DENIED                                               = Just "ERROR_DS_DRA_ACCESS_DENIED"
displaySystemErrorCode' ERROR_DS_DRA_NOT_SUPPORTED                                               = Just "ERROR_DS_DRA_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_DS_DRA_RPC_CANCELLED                                               = Just "ERROR_DS_DRA_RPC_CANCELLED"
displaySystemErrorCode' ERROR_DS_DRA_SOURCE_DISABLED                                             = Just "ERROR_DS_DRA_SOURCE_DISABLED"
displaySystemErrorCode' ERROR_DS_DRA_SINK_DISABLED                                               = Just "ERROR_DS_DRA_SINK_DISABLED"
displaySystemErrorCode' ERROR_DS_DRA_NAME_COLLISION                                              = Just "ERROR_DS_DRA_NAME_COLLISION"
displaySystemErrorCode' ERROR_DS_DRA_SOURCE_REINSTALLED                                          = Just "ERROR_DS_DRA_SOURCE_REINSTALLED"
displaySystemErrorCode' ERROR_DS_DRA_MISSING_PARENT                                              = Just "ERROR_DS_DRA_MISSING_PARENT"
displaySystemErrorCode' ERROR_DS_DRA_PREEMPTED                                                   = Just "ERROR_DS_DRA_PREEMPTED"
displaySystemErrorCode' ERROR_DS_DRA_ABANDON_SYNC                                                = Just "ERROR_DS_DRA_ABANDON_SYNC"
displaySystemErrorCode' ERROR_DS_DRA_SHUTDOWN                                                    = Just "ERROR_DS_DRA_SHUTDOWN"
displaySystemErrorCode' ERROR_DS_DRA_INCOMPATIBLE_PARTIAL_SET                                    = Just "ERROR_DS_DRA_INCOMPATIBLE_PARTIAL_SET"
displaySystemErrorCode' ERROR_DS_DRA_SOURCE_IS_PARTIAL_REPLICA                                   = Just "ERROR_DS_DRA_SOURCE_IS_PARTIAL_REPLICA"
displaySystemErrorCode' ERROR_DS_DRA_EXTN_CONNECTION_FAILED                                      = Just "ERROR_DS_DRA_EXTN_CONNECTION_FAILED"
displaySystemErrorCode' ERROR_DS_INSTALL_SCHEMA_MISMATCH                                         = Just "ERROR_DS_INSTALL_SCHEMA_MISMATCH"
displaySystemErrorCode' ERROR_DS_DUP_LINK_ID                                                     = Just "ERROR_DS_DUP_LINK_ID"
displaySystemErrorCode' ERROR_DS_NAME_ERROR_RESOLVING                                            = Just "ERROR_DS_NAME_ERROR_RESOLVING"
displaySystemErrorCode' ERROR_DS_NAME_ERROR_NOT_FOUND                                            = Just "ERROR_DS_NAME_ERROR_NOT_FOUND"
displaySystemErrorCode' ERROR_DS_NAME_ERROR_NOT_UNIQUE                                           = Just "ERROR_DS_NAME_ERROR_NOT_UNIQUE"
displaySystemErrorCode' ERROR_DS_NAME_ERROR_NO_MAPPING                                           = Just "ERROR_DS_NAME_ERROR_NO_MAPPING"
displaySystemErrorCode' ERROR_DS_NAME_ERROR_DOMAIN_ONLY                                          = Just "ERROR_DS_NAME_ERROR_DOMAIN_ONLY"
displaySystemErrorCode' ERROR_DS_NAME_ERROR_NO_SYNTACTICAL_MAPPING                               = Just "ERROR_DS_NAME_ERROR_NO_SYNTACTICAL_MAPPING"
displaySystemErrorCode' ERROR_DS_CONSTRUCTED_ATT_MOD                                             = Just "ERROR_DS_CONSTRUCTED_ATT_MOD"
displaySystemErrorCode' ERROR_DS_WRONG_OM_OBJ_CLASS                                              = Just "ERROR_DS_WRONG_OM_OBJ_CLASS"
displaySystemErrorCode' ERROR_DS_DRA_REPL_PENDING                                                = Just "ERROR_DS_DRA_REPL_PENDING"
displaySystemErrorCode' ERROR_DS_DS_REQUIRED                                                     = Just "ERROR_DS_DS_REQUIRED"
displaySystemErrorCode' ERROR_DS_INVALID_LDAP_DISPLAY_NAME                                       = Just "ERROR_DS_INVALID_LDAP_DISPLAY_NAME"
displaySystemErrorCode' ERROR_DS_NON_BASE_SEARCH                                                 = Just "ERROR_DS_NON_BASE_SEARCH"
displaySystemErrorCode' ERROR_DS_CANT_RETRIEVE_ATTS                                              = Just "ERROR_DS_CANT_RETRIEVE_ATTS"
displaySystemErrorCode' ERROR_DS_BACKLINK_WITHOUT_LINK                                           = Just "ERROR_DS_BACKLINK_WITHOUT_LINK"
displaySystemErrorCode' ERROR_DS_EPOCH_MISMATCH                                                  = Just "ERROR_DS_EPOCH_MISMATCH"
displaySystemErrorCode' ERROR_DS_SRC_NAME_MISMATCH                                               = Just "ERROR_DS_SRC_NAME_MISMATCH"
displaySystemErrorCode' ERROR_DS_SRC_AND_DST_NC_IDENTICAL                                        = Just "ERROR_DS_SRC_AND_DST_NC_IDENTICAL"
displaySystemErrorCode' ERROR_DS_DST_NC_MISMATCH                                                 = Just "ERROR_DS_DST_NC_MISMATCH"
displaySystemErrorCode' ERROR_DS_NOT_AUTHORITIVE_FOR_DST_NC                                      = Just "ERROR_DS_NOT_AUTHORITIVE_FOR_DST_NC"
displaySystemErrorCode' ERROR_DS_SRC_GUID_MISMATCH                                               = Just "ERROR_DS_SRC_GUID_MISMATCH"
displaySystemErrorCode' ERROR_DS_CANT_MOVE_DELETED_OBJECT                                        = Just "ERROR_DS_CANT_MOVE_DELETED_OBJECT"
displaySystemErrorCode' ERROR_DS_PDC_OPERATION_IN_PROGRESS                                       = Just "ERROR_DS_PDC_OPERATION_IN_PROGRESS"
displaySystemErrorCode' ERROR_DS_CROSS_DOMAIN_CLEANUP_REQD                                       = Just "ERROR_DS_CROSS_DOMAIN_CLEANUP_REQD"
displaySystemErrorCode' ERROR_DS_ILLEGAL_XDOM_MOVE_OPERATION                                     = Just "ERROR_DS_ILLEGAL_XDOM_MOVE_OPERATION"
displaySystemErrorCode' ERROR_DS_CANT_WITH_ACCT_GROUP_MEMBERSHPS                                 = Just "ERROR_DS_CANT_WITH_ACCT_GROUP_MEMBERSHPS"
displaySystemErrorCode' ERROR_DS_NC_MUST_HAVE_NC_PARENT                                          = Just "ERROR_DS_NC_MUST_HAVE_NC_PARENT"
displaySystemErrorCode' ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE                                       = Just "ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE"
displaySystemErrorCode' ERROR_DS_DST_DOMAIN_NOT_NATIVE                                           = Just "ERROR_DS_DST_DOMAIN_NOT_NATIVE"
displaySystemErrorCode' ERROR_DS_MISSING_INFRASTRUCTURE_CONTAINER                                = Just "ERROR_DS_MISSING_INFRASTRUCTURE_CONTAINER"
displaySystemErrorCode' ERROR_DS_CANT_MOVE_ACCOUNT_GROUP                                         = Just "ERROR_DS_CANT_MOVE_ACCOUNT_GROUP"
displaySystemErrorCode' ERROR_DS_CANT_MOVE_RESOURCE_GROUP                                        = Just "ERROR_DS_CANT_MOVE_RESOURCE_GROUP"
displaySystemErrorCode' ERROR_DS_INVALID_SEARCH_FLAG                                             = Just "ERROR_DS_INVALID_SEARCH_FLAG"
displaySystemErrorCode' ERROR_DS_NO_TREE_DELETE_ABOVE_NC                                         = Just "ERROR_DS_NO_TREE_DELETE_ABOVE_NC"
displaySystemErrorCode' ERROR_DS_COULDNT_LOCK_TREE_FOR_DELETE                                    = Just "ERROR_DS_COULDNT_LOCK_TREE_FOR_DELETE"
displaySystemErrorCode' ERROR_DS_COULDNT_IDENTIFY_OBJECTS_FOR_TREE_DELETE                        = Just "ERROR_DS_COULDNT_IDENTIFY_OBJECTS_FOR_TREE_DELETE"
displaySystemErrorCode' ERROR_DS_SAM_INIT_FAILURE                                                = Just "ERROR_DS_SAM_INIT_FAILURE"
displaySystemErrorCode' ERROR_DS_SENSITIVE_GROUP_VIOLATION                                       = Just "ERROR_DS_SENSITIVE_GROUP_VIOLATION"
displaySystemErrorCode' ERROR_DS_CANT_MOD_PRIMARYGROUPID                                         = Just "ERROR_DS_CANT_MOD_PRIMARYGROUPID"
displaySystemErrorCode' ERROR_DS_ILLEGAL_BASE_SCHEMA_MOD                                         = Just "ERROR_DS_ILLEGAL_BASE_SCHEMA_MOD"
displaySystemErrorCode' ERROR_DS_NONSAFE_SCHEMA_CHANGE                                           = Just "ERROR_DS_NONSAFE_SCHEMA_CHANGE"
displaySystemErrorCode' ERROR_DS_SCHEMA_UPDATE_DISALLOWED                                        = Just "ERROR_DS_SCHEMA_UPDATE_DISALLOWED"
displaySystemErrorCode' ERROR_DS_CANT_CREATE_UNDER_SCHEMA                                        = Just "ERROR_DS_CANT_CREATE_UNDER_SCHEMA"
displaySystemErrorCode' ERROR_DS_INSTALL_NO_SRC_SCH_VERSION                                      = Just "ERROR_DS_INSTALL_NO_SRC_SCH_VERSION"
displaySystemErrorCode' ERROR_DS_INSTALL_NO_SCH_VERSION_IN_INIFILE                               = Just "ERROR_DS_INSTALL_NO_SCH_VERSION_IN_INIFILE"
displaySystemErrorCode' ERROR_DS_INVALID_GROUP_TYPE                                              = Just "ERROR_DS_INVALID_GROUP_TYPE"
displaySystemErrorCode' ERROR_DS_NO_NEST_GLOBALGROUP_IN_MIXEDDOMAIN                              = Just "ERROR_DS_NO_NEST_GLOBALGROUP_IN_MIXEDDOMAIN"
displaySystemErrorCode' ERROR_DS_NO_NEST_LOCALGROUP_IN_MIXEDDOMAIN                               = Just "ERROR_DS_NO_NEST_LOCALGROUP_IN_MIXEDDOMAIN"
displaySystemErrorCode' ERROR_DS_GLOBAL_CANT_HAVE_LOCAL_MEMBER                                   = Just "ERROR_DS_GLOBAL_CANT_HAVE_LOCAL_MEMBER"
displaySystemErrorCode' ERROR_DS_GLOBAL_CANT_HAVE_UNIVERSAL_MEMBER                               = Just "ERROR_DS_GLOBAL_CANT_HAVE_UNIVERSAL_MEMBER"
displaySystemErrorCode' ERROR_DS_UNIVERSAL_CANT_HAVE_LOCAL_MEMBER                                = Just "ERROR_DS_UNIVERSAL_CANT_HAVE_LOCAL_MEMBER"
displaySystemErrorCode' ERROR_DS_GLOBAL_CANT_HAVE_CROSSDOMAIN_MEMBER                             = Just "ERROR_DS_GLOBAL_CANT_HAVE_CROSSDOMAIN_MEMBER"
displaySystemErrorCode' ERROR_DS_LOCAL_CANT_HAVE_CROSSDOMAIN_LOCAL_MEMBER                        = Just "ERROR_DS_LOCAL_CANT_HAVE_CROSSDOMAIN_LOCAL_MEMBER"
displaySystemErrorCode' ERROR_DS_HAVE_PRIMARY_MEMBERS                                            = Just "ERROR_DS_HAVE_PRIMARY_MEMBERS"
displaySystemErrorCode' ERROR_DS_STRING_SD_CONVERSION_FAILED                                     = Just "ERROR_DS_STRING_SD_CONVERSION_FAILED"
displaySystemErrorCode' ERROR_DS_NAMING_MASTER_GC                                                = Just "ERROR_DS_NAMING_MASTER_GC"
displaySystemErrorCode' ERROR_DS_DNS_LOOKUP_FAILURE                                              = Just "ERROR_DS_DNS_LOOKUP_FAILURE"
displaySystemErrorCode' ERROR_DS_COULDNT_UPDATE_SPNS                                             = Just "ERROR_DS_COULDNT_UPDATE_SPNS"
displaySystemErrorCode' ERROR_DS_CANT_RETRIEVE_SD                                                = Just "ERROR_DS_CANT_RETRIEVE_SD"
displaySystemErrorCode' ERROR_DS_KEY_NOT_UNIQUE                                                  = Just "ERROR_DS_KEY_NOT_UNIQUE"
displaySystemErrorCode' ERROR_DS_WRONG_LINKED_ATT_SYNTAX                                         = Just "ERROR_DS_WRONG_LINKED_ATT_SYNTAX"
displaySystemErrorCode' ERROR_DS_SAM_NEED_BOOTKEY_PASSWORD                                       = Just "ERROR_DS_SAM_NEED_BOOTKEY_PASSWORD"
displaySystemErrorCode' ERROR_DS_SAM_NEED_BOOTKEY_FLOPPY                                         = Just "ERROR_DS_SAM_NEED_BOOTKEY_FLOPPY"
displaySystemErrorCode' ERROR_DS_CANT_START                                                      = Just "ERROR_DS_CANT_START"
displaySystemErrorCode' ERROR_DS_INIT_FAILURE                                                    = Just "ERROR_DS_INIT_FAILURE"
displaySystemErrorCode' ERROR_DS_NO_PKT_PRIVACY_ON_CONNECTION                                    = Just "ERROR_DS_NO_PKT_PRIVACY_ON_CONNECTION"
displaySystemErrorCode' ERROR_DS_SOURCE_DOMAIN_IN_FOREST                                         = Just "ERROR_DS_SOURCE_DOMAIN_IN_FOREST"
displaySystemErrorCode' ERROR_DS_DESTINATION_DOMAIN_NOT_IN_FOREST                                = Just "ERROR_DS_DESTINATION_DOMAIN_NOT_IN_FOREST"
displaySystemErrorCode' ERROR_DS_DESTINATION_AUDITING_NOT_ENABLED                                = Just "ERROR_DS_DESTINATION_AUDITING_NOT_ENABLED"
displaySystemErrorCode' ERROR_DS_CANT_FIND_DC_FOR_SRC_DOMAIN                                     = Just "ERROR_DS_CANT_FIND_DC_FOR_SRC_DOMAIN"
displaySystemErrorCode' ERROR_DS_SRC_OBJ_NOT_GROUP_OR_USER                                       = Just "ERROR_DS_SRC_OBJ_NOT_GROUP_OR_USER"
displaySystemErrorCode' ERROR_DS_SRC_SID_EXISTS_IN_FOREST                                        = Just "ERROR_DS_SRC_SID_EXISTS_IN_FOREST"
displaySystemErrorCode' ERROR_DS_SRC_AND_DST_OBJECT_CLASS_MISMATCH                               = Just "ERROR_DS_SRC_AND_DST_OBJECT_CLASS_MISMATCH"
displaySystemErrorCode' ERROR_SAM_INIT_FAILURE                                                   = Just "ERROR_SAM_INIT_FAILURE"
displaySystemErrorCode' ERROR_DS_DRA_SCHEMA_INFO_SHIP                                            = Just "ERROR_DS_DRA_SCHEMA_INFO_SHIP"
displaySystemErrorCode' ERROR_DS_DRA_SCHEMA_CONFLICT                                             = Just "ERROR_DS_DRA_SCHEMA_CONFLICT"
displaySystemErrorCode' ERROR_DS_DRA_EARLIER_SCHEMA_CONFLICT                                     = Just "ERROR_DS_DRA_EARLIER_SCHEMA_CONFLICT"
displaySystemErrorCode' ERROR_DS_DRA_OBJ_NC_MISMATCH                                             = Just "ERROR_DS_DRA_OBJ_NC_MISMATCH"
displaySystemErrorCode' ERROR_DS_NC_STILL_HAS_DSAS                                               = Just "ERROR_DS_NC_STILL_HAS_DSAS"
displaySystemErrorCode' ERROR_DS_GC_REQUIRED                                                     = Just "ERROR_DS_GC_REQUIRED"
displaySystemErrorCode' ERROR_DS_LOCAL_MEMBER_OF_LOCAL_ONLY                                      = Just "ERROR_DS_LOCAL_MEMBER_OF_LOCAL_ONLY"
displaySystemErrorCode' ERROR_DS_NO_FPO_IN_UNIVERSAL_GROUPS                                      = Just "ERROR_DS_NO_FPO_IN_UNIVERSAL_GROUPS"
displaySystemErrorCode' ERROR_DS_CANT_ADD_TO_GC                                                  = Just "ERROR_DS_CANT_ADD_TO_GC"
displaySystemErrorCode' ERROR_DS_NO_CHECKPOINT_WITH_PDC                                          = Just "ERROR_DS_NO_CHECKPOINT_WITH_PDC"
displaySystemErrorCode' ERROR_DS_SOURCE_AUDITING_NOT_ENABLED                                     = Just "ERROR_DS_SOURCE_AUDITING_NOT_ENABLED"
displaySystemErrorCode' ERROR_DS_CANT_CREATE_IN_NONDOMAIN_NC                                     = Just "ERROR_DS_CANT_CREATE_IN_NONDOMAIN_NC"
displaySystemErrorCode' ERROR_DS_INVALID_NAME_FOR_SPN                                            = Just "ERROR_DS_INVALID_NAME_FOR_SPN"
displaySystemErrorCode' ERROR_DS_FILTER_USES_CONTRUCTED_ATTRS                                    = Just "ERROR_DS_FILTER_USES_CONTRUCTED_ATTRS"
displaySystemErrorCode' ERROR_DS_UNICODEPWD_NOT_IN_QUOTES                                        = Just "ERROR_DS_UNICODEPWD_NOT_IN_QUOTES"
displaySystemErrorCode' ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED                                  = Just "ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"
displaySystemErrorCode' ERROR_DS_MUST_BE_RUN_ON_DST_DC                                           = Just "ERROR_DS_MUST_BE_RUN_ON_DST_DC"
displaySystemErrorCode' ERROR_DS_SRC_DC_MUST_BE_SP4_OR_GREATER                                   = Just "ERROR_DS_SRC_DC_MUST_BE_SP4_OR_GREATER"
displaySystemErrorCode' ERROR_DS_CANT_TREE_DELETE_CRITICAL_OBJ                                   = Just "ERROR_DS_CANT_TREE_DELETE_CRITICAL_OBJ"
displaySystemErrorCode' ERROR_DS_INIT_FAILURE_CONSOLE                                            = Just "ERROR_DS_INIT_FAILURE_CONSOLE"
displaySystemErrorCode' ERROR_DS_SAM_INIT_FAILURE_CONSOLE                                        = Just "ERROR_DS_SAM_INIT_FAILURE_CONSOLE"
displaySystemErrorCode' ERROR_DS_FOREST_VERSION_TOO_HIGH                                         = Just "ERROR_DS_FOREST_VERSION_TOO_HIGH"
displaySystemErrorCode' ERROR_DS_DOMAIN_VERSION_TOO_HIGH                                         = Just "ERROR_DS_DOMAIN_VERSION_TOO_HIGH"
displaySystemErrorCode' ERROR_DS_FOREST_VERSION_TOO_LOW                                          = Just "ERROR_DS_FOREST_VERSION_TOO_LOW"
displaySystemErrorCode' ERROR_DS_DOMAIN_VERSION_TOO_LOW                                          = Just "ERROR_DS_DOMAIN_VERSION_TOO_LOW"
displaySystemErrorCode' ERROR_DS_INCOMPATIBLE_VERSION                                            = Just "ERROR_DS_INCOMPATIBLE_VERSION"
displaySystemErrorCode' ERROR_DS_LOW_DSA_VERSION                                                 = Just "ERROR_DS_LOW_DSA_VERSION"
displaySystemErrorCode' ERROR_DS_NO_BEHAVIOR_VERSION_IN_MIXEDDOMAIN                              = Just "ERROR_DS_NO_BEHAVIOR_VERSION_IN_MIXEDDOMAIN"
displaySystemErrorCode' ERROR_DS_NOT_SUPPORTED_SORT_ORDER                                        = Just "ERROR_DS_NOT_SUPPORTED_SORT_ORDER"
displaySystemErrorCode' ERROR_DS_NAME_NOT_UNIQUE                                                 = Just "ERROR_DS_NAME_NOT_UNIQUE"
displaySystemErrorCode' ERROR_DS_MACHINE_ACCOUNT_CREATED_PRENT4                                  = Just "ERROR_DS_MACHINE_ACCOUNT_CREATED_PRENT4"
displaySystemErrorCode' ERROR_DS_OUT_OF_VERSION_STORE                                            = Just "ERROR_DS_OUT_OF_VERSION_STORE"
displaySystemErrorCode' ERROR_DS_INCOMPATIBLE_CONTROLS_USED                                      = Just "ERROR_DS_INCOMPATIBLE_CONTROLS_USED"
displaySystemErrorCode' ERROR_DS_NO_REF_DOMAIN                                                   = Just "ERROR_DS_NO_REF_DOMAIN"
displaySystemErrorCode' ERROR_DS_RESERVED_LINK_ID                                                = Just "ERROR_DS_RESERVED_LINK_ID"
displaySystemErrorCode' ERROR_DS_LINK_ID_NOT_AVAILABLE                                           = Just "ERROR_DS_LINK_ID_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_DS_AG_CANT_HAVE_UNIVERSAL_MEMBER                                   = Just "ERROR_DS_AG_CANT_HAVE_UNIVERSAL_MEMBER"
displaySystemErrorCode' ERROR_DS_MODIFYDN_DISALLOWED_BY_INSTANCE_TYPE                            = Just "ERROR_DS_MODIFYDN_DISALLOWED_BY_INSTANCE_TYPE"
displaySystemErrorCode' ERROR_DS_NO_OBJECT_MOVE_IN_SCHEMA_NC                                     = Just "ERROR_DS_NO_OBJECT_MOVE_IN_SCHEMA_NC"
displaySystemErrorCode' ERROR_DS_MODIFYDN_DISALLOWED_BY_FLAG                                     = Just "ERROR_DS_MODIFYDN_DISALLOWED_BY_FLAG"
displaySystemErrorCode' ERROR_DS_MODIFYDN_WRONG_GRANDPARENT                                      = Just "ERROR_DS_MODIFYDN_WRONG_GRANDPARENT"
displaySystemErrorCode' ERROR_DS_NAME_ERROR_TRUST_REFERRAL                                       = Just "ERROR_DS_NAME_ERROR_TRUST_REFERRAL"
displaySystemErrorCode' ERROR_NOT_SUPPORTED_ON_STANDARD_SERVER                                   = Just "ERROR_NOT_SUPPORTED_ON_STANDARD_SERVER"
displaySystemErrorCode' ERROR_DS_CANT_ACCESS_REMOTE_PART_OF_AD                                   = Just "ERROR_DS_CANT_ACCESS_REMOTE_PART_OF_AD"
displaySystemErrorCode' ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE_V2                                    = Just "ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE_V2"
displaySystemErrorCode' ERROR_DS_THREAD_LIMIT_EXCEEDED                                           = Just "ERROR_DS_THREAD_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_DS_NOT_CLOSEST                                                     = Just "ERROR_DS_NOT_CLOSEST"
displaySystemErrorCode' ERROR_DS_CANT_DERIVE_SPN_WITHOUT_SERVER_REF                              = Just "ERROR_DS_CANT_DERIVE_SPN_WITHOUT_SERVER_REF"
displaySystemErrorCode' ERROR_DS_SINGLE_USER_MODE_FAILED                                         = Just "ERROR_DS_SINGLE_USER_MODE_FAILED"
displaySystemErrorCode' ERROR_DS_NTDSCRIPT_SYNTAX_ERROR                                          = Just "ERROR_DS_NTDSCRIPT_SYNTAX_ERROR"
displaySystemErrorCode' ERROR_DS_NTDSCRIPT_PROCESS_ERROR                                         = Just "ERROR_DS_NTDSCRIPT_PROCESS_ERROR"
displaySystemErrorCode' ERROR_DS_DIFFERENT_REPL_EPOCHS                                           = Just "ERROR_DS_DIFFERENT_REPL_EPOCHS"
displaySystemErrorCode' ERROR_DS_DRS_EXTENSIONS_CHANGED                                          = Just "ERROR_DS_DRS_EXTENSIONS_CHANGED"
displaySystemErrorCode' ERROR_DS_REPLICA_SET_CHANGE_NOT_ALLOWED_ON_DISABLED_CR                   = Just "ERROR_DS_REPLICA_SET_CHANGE_NOT_ALLOWED_ON_DISABLED_CR"
displaySystemErrorCode' ERROR_DS_NO_MSDS_INTID                                                   = Just "ERROR_DS_NO_MSDS_INTID"
displaySystemErrorCode' ERROR_DS_DUP_MSDS_INTID                                                  = Just "ERROR_DS_DUP_MSDS_INTID"
displaySystemErrorCode' ERROR_DS_EXISTS_IN_RDNATTID                                              = Just "ERROR_DS_EXISTS_IN_RDNATTID"
displaySystemErrorCode' ERROR_DS_AUTHORIZATION_FAILED                                            = Just "ERROR_DS_AUTHORIZATION_FAILED"
displaySystemErrorCode' ERROR_DS_INVALID_SCRIPT                                                  = Just "ERROR_DS_INVALID_SCRIPT"
displaySystemErrorCode' ERROR_DS_REMOTE_CROSSREF_OP_FAILED                                       = Just "ERROR_DS_REMOTE_CROSSREF_OP_FAILED"
displaySystemErrorCode' ERROR_DS_CROSS_REF_BUSY                                                  = Just "ERROR_DS_CROSS_REF_BUSY"
displaySystemErrorCode' ERROR_DS_CANT_DERIVE_SPN_FOR_DELETED_DOMAIN                              = Just "ERROR_DS_CANT_DERIVE_SPN_FOR_DELETED_DOMAIN"
displaySystemErrorCode' ERROR_DS_CANT_DEMOTE_WITH_WRITEABLE_NC                                   = Just "ERROR_DS_CANT_DEMOTE_WITH_WRITEABLE_NC"
displaySystemErrorCode' ERROR_DS_DUPLICATE_ID_FOUND                                              = Just "ERROR_DS_DUPLICATE_ID_FOUND"
displaySystemErrorCode' ERROR_DS_INSUFFICIENT_ATTR_TO_CREATE_OBJECT                              = Just "ERROR_DS_INSUFFICIENT_ATTR_TO_CREATE_OBJECT"
displaySystemErrorCode' ERROR_DS_GROUP_CONVERSION_ERROR                                          = Just "ERROR_DS_GROUP_CONVERSION_ERROR"
displaySystemErrorCode' ERROR_DS_CANT_MOVE_APP_BASIC_GROUP                                       = Just "ERROR_DS_CANT_MOVE_APP_BASIC_GROUP"
displaySystemErrorCode' ERROR_DS_CANT_MOVE_APP_QUERY_GROUP                                       = Just "ERROR_DS_CANT_MOVE_APP_QUERY_GROUP"
displaySystemErrorCode' ERROR_DS_ROLE_NOT_VERIFIED                                               = Just "ERROR_DS_ROLE_NOT_VERIFIED"
displaySystemErrorCode' ERROR_DS_WKO_CONTAINER_CANNOT_BE_SPECIAL                                 = Just "ERROR_DS_WKO_CONTAINER_CANNOT_BE_SPECIAL"
displaySystemErrorCode' ERROR_DS_DOMAIN_RENAME_IN_PROGRESS                                       = Just "ERROR_DS_DOMAIN_RENAME_IN_PROGRESS"
displaySystemErrorCode' ERROR_DS_EXISTING_AD_CHILD_NC                                            = Just "ERROR_DS_EXISTING_AD_CHILD_NC"
displaySystemErrorCode' ERROR_DS_REPL_LIFETIME_EXCEEDED                                          = Just "ERROR_DS_REPL_LIFETIME_EXCEEDED"
displaySystemErrorCode' ERROR_DS_DISALLOWED_IN_SYSTEM_CONTAINER                                  = Just "ERROR_DS_DISALLOWED_IN_SYSTEM_CONTAINER"
displaySystemErrorCode' ERROR_DS_LDAP_SEND_QUEUE_FULL                                            = Just "ERROR_DS_LDAP_SEND_QUEUE_FULL"
displaySystemErrorCode' ERROR_DS_DRA_OUT_SCHEDULE_WINDOW                                         = Just "ERROR_DS_DRA_OUT_SCHEDULE_WINDOW"
displaySystemErrorCode' ERROR_DS_POLICY_NOT_KNOWN                                                = Just "ERROR_DS_POLICY_NOT_KNOWN"
displaySystemErrorCode' ERROR_NO_SITE_SETTINGS_OBJECT                                            = Just "ERROR_NO_SITE_SETTINGS_OBJECT"
displaySystemErrorCode' ERROR_NO_SECRETS                                                         = Just "ERROR_NO_SECRETS"
displaySystemErrorCode' ERROR_NO_WRITABLE_DC_FOUND                                               = Just "ERROR_NO_WRITABLE_DC_FOUND"
displaySystemErrorCode' ERROR_DS_NO_SERVER_OBJECT                                                = Just "ERROR_DS_NO_SERVER_OBJECT"
displaySystemErrorCode' ERROR_DS_NO_NTDSA_OBJECT                                                 = Just "ERROR_DS_NO_NTDSA_OBJECT"
displaySystemErrorCode' ERROR_DS_NON_ASQ_SEARCH                                                  = Just "ERROR_DS_NON_ASQ_SEARCH"
displaySystemErrorCode' ERROR_DS_AUDIT_FAILURE                                                   = Just "ERROR_DS_AUDIT_FAILURE"
displaySystemErrorCode' ERROR_DS_INVALID_SEARCH_FLAG_SUBTREE                                     = Just "ERROR_DS_INVALID_SEARCH_FLAG_SUBTREE"
displaySystemErrorCode' ERROR_DS_INVALID_SEARCH_FLAG_TUPLE                                       = Just "ERROR_DS_INVALID_SEARCH_FLAG_TUPLE"
displaySystemErrorCode' ERROR_DS_HIERARCHY_TABLE_TOO_DEEP                                        = Just "ERROR_DS_HIERARCHY_TABLE_TOO_DEEP"
displaySystemErrorCode' ERROR_DS_DRA_CORRUPT_UTD_VECTOR                                          = Just "ERROR_DS_DRA_CORRUPT_UTD_VECTOR"
displaySystemErrorCode' ERROR_DS_DRA_SECRETS_DENIED                                              = Just "ERROR_DS_DRA_SECRETS_DENIED"
displaySystemErrorCode' ERROR_DS_RESERVED_MAPI_ID                                                = Just "ERROR_DS_RESERVED_MAPI_ID"
displaySystemErrorCode' ERROR_DS_MAPI_ID_NOT_AVAILABLE                                           = Just "ERROR_DS_MAPI_ID_NOT_AVAILABLE"
displaySystemErrorCode' ERROR_DS_DRA_MISSING_KRBTGT_SECRET                                       = Just "ERROR_DS_DRA_MISSING_KRBTGT_SECRET"
displaySystemErrorCode' ERROR_DS_DOMAIN_NAME_EXISTS_IN_FOREST                                    = Just "ERROR_DS_DOMAIN_NAME_EXISTS_IN_FOREST"
displaySystemErrorCode' ERROR_DS_FLAT_NAME_EXISTS_IN_FOREST                                      = Just "ERROR_DS_FLAT_NAME_EXISTS_IN_FOREST"
displaySystemErrorCode' ERROR_INVALID_USER_PRINCIPAL_NAME                                        = Just "ERROR_INVALID_USER_PRINCIPAL_NAME"
displaySystemErrorCode' ERROR_DS_OID_MAPPED_GROUP_CANT_HAVE_MEMBERS                              = Just "ERROR_DS_OID_MAPPED_GROUP_CANT_HAVE_MEMBERS"
displaySystemErrorCode' ERROR_DS_OID_NOT_FOUND                                                   = Just "ERROR_DS_OID_NOT_FOUND"
displaySystemErrorCode' ERROR_DS_DRA_RECYCLED_TARGET                                             = Just "ERROR_DS_DRA_RECYCLED_TARGET"
displaySystemErrorCode' ERROR_DS_DISALLOWED_NC_REDIRECT                                          = Just "ERROR_DS_DISALLOWED_NC_REDIRECT"
displaySystemErrorCode' ERROR_DS_HIGH_ADLDS_FFL                                                  = Just "ERROR_DS_HIGH_ADLDS_FFL"
displaySystemErrorCode' ERROR_DS_HIGH_DSA_VERSION                                                = Just "ERROR_DS_HIGH_DSA_VERSION"
displaySystemErrorCode' ERROR_DS_LOW_ADLDS_FFL                                                   = Just "ERROR_DS_LOW_ADLDS_FFL"
displaySystemErrorCode' ERROR_DOMAIN_SID_SAME_AS_LOCAL_WORKSTATION                               = Just "ERROR_DOMAIN_SID_SAME_AS_LOCAL_WORKSTATION"
displaySystemErrorCode' ERROR_DS_UNDELETE_SAM_VALIDATION_FAILED                                  = Just "ERROR_DS_UNDELETE_SAM_VALIDATION_FAILED"
displaySystemErrorCode' ERROR_INCORRECT_ACCOUNT_TYPE                                             = Just "ERROR_INCORRECT_ACCOUNT_TYPE"
displaySystemErrorCode' ERROR_DS_SPN_VALUE_NOT_UNIQUE_IN_FOREST                                  = Just "ERROR_DS_SPN_VALUE_NOT_UNIQUE_IN_FOREST"
displaySystemErrorCode' ERROR_DS_UPN_VALUE_NOT_UNIQUE_IN_FOREST                                  = Just "ERROR_DS_UPN_VALUE_NOT_UNIQUE_IN_FOREST"
displaySystemErrorCode' ERROR_IPSEC_QM_POLICY_EXISTS                                             = Just "ERROR_IPSEC_QM_POLICY_EXISTS"
displaySystemErrorCode' ERROR_IPSEC_QM_POLICY_NOT_FOUND                                          = Just "ERROR_IPSEC_QM_POLICY_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_QM_POLICY_IN_USE                                             = Just "ERROR_IPSEC_QM_POLICY_IN_USE"
displaySystemErrorCode' ERROR_IPSEC_MM_POLICY_EXISTS                                             = Just "ERROR_IPSEC_MM_POLICY_EXISTS"
displaySystemErrorCode' ERROR_IPSEC_MM_POLICY_NOT_FOUND                                          = Just "ERROR_IPSEC_MM_POLICY_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_MM_POLICY_IN_USE                                             = Just "ERROR_IPSEC_MM_POLICY_IN_USE"
displaySystemErrorCode' ERROR_IPSEC_MM_FILTER_EXISTS                                             = Just "ERROR_IPSEC_MM_FILTER_EXISTS"
displaySystemErrorCode' ERROR_IPSEC_MM_FILTER_NOT_FOUND                                          = Just "ERROR_IPSEC_MM_FILTER_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_TRANSPORT_FILTER_EXISTS                                      = Just "ERROR_IPSEC_TRANSPORT_FILTER_EXISTS"
displaySystemErrorCode' ERROR_IPSEC_TRANSPORT_FILTER_NOT_FOUND                                   = Just "ERROR_IPSEC_TRANSPORT_FILTER_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_MM_AUTH_EXISTS                                               = Just "ERROR_IPSEC_MM_AUTH_EXISTS"
displaySystemErrorCode' ERROR_IPSEC_MM_AUTH_NOT_FOUND                                            = Just "ERROR_IPSEC_MM_AUTH_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_MM_AUTH_IN_USE                                               = Just "ERROR_IPSEC_MM_AUTH_IN_USE"
displaySystemErrorCode' ERROR_IPSEC_DEFAULT_MM_POLICY_NOT_FOUND                                  = Just "ERROR_IPSEC_DEFAULT_MM_POLICY_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_DEFAULT_MM_AUTH_NOT_FOUND                                    = Just "ERROR_IPSEC_DEFAULT_MM_AUTH_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_DEFAULT_QM_POLICY_NOT_FOUND                                  = Just "ERROR_IPSEC_DEFAULT_QM_POLICY_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_TUNNEL_FILTER_EXISTS                                         = Just "ERROR_IPSEC_TUNNEL_FILTER_EXISTS"
displaySystemErrorCode' ERROR_IPSEC_TUNNEL_FILTER_NOT_FOUND                                      = Just "ERROR_IPSEC_TUNNEL_FILTER_NOT_FOUND"
displaySystemErrorCode' ERROR_IPSEC_MM_FILTER_PENDING_DELETION                                   = Just "ERROR_IPSEC_MM_FILTER_PENDING_DELETION"
displaySystemErrorCode' ERROR_IPSEC_TRANSPORT_FILTER_PENDING_DELETION                            = Just "ERROR_IPSEC_TRANSPORT_FILTER_PENDING_DELETION"
displaySystemErrorCode' ERROR_IPSEC_TUNNEL_FILTER_PENDING_DELETION                               = Just "ERROR_IPSEC_TUNNEL_FILTER_PENDING_DELETION"
displaySystemErrorCode' ERROR_IPSEC_MM_POLICY_PENDING_DELETION                                   = Just "ERROR_IPSEC_MM_POLICY_PENDING_DELETION"
displaySystemErrorCode' ERROR_IPSEC_MM_AUTH_PENDING_DELETION                                     = Just "ERROR_IPSEC_MM_AUTH_PENDING_DELETION"
displaySystemErrorCode' ERROR_IPSEC_QM_POLICY_PENDING_DELETION                                   = Just "ERROR_IPSEC_QM_POLICY_PENDING_DELETION"
displaySystemErrorCode' ERROR_IPSEC_IKE_NEG_STATUS_BEGIN                                         = Just "ERROR_IPSEC_IKE_NEG_STATUS_BEGIN"
displaySystemErrorCode' ERROR_IPSEC_IKE_AUTH_FAIL                                                = Just "ERROR_IPSEC_IKE_AUTH_FAIL"
displaySystemErrorCode' ERROR_IPSEC_IKE_ATTRIB_FAIL                                              = Just "ERROR_IPSEC_IKE_ATTRIB_FAIL"
displaySystemErrorCode' ERROR_IPSEC_IKE_NEGOTIATION_PENDING                                      = Just "ERROR_IPSEC_IKE_NEGOTIATION_PENDING"
displaySystemErrorCode' ERROR_IPSEC_IKE_GENERAL_PROCESSING_ERROR                                 = Just "ERROR_IPSEC_IKE_GENERAL_PROCESSING_ERROR"
displaySystemErrorCode' ERROR_IPSEC_IKE_TIMED_OUT                                                = Just "ERROR_IPSEC_IKE_TIMED_OUT"
displaySystemErrorCode' ERROR_IPSEC_IKE_NO_CERT                                                  = Just "ERROR_IPSEC_IKE_NO_CERT"
displaySystemErrorCode' ERROR_IPSEC_IKE_SA_DELETED                                               = Just "ERROR_IPSEC_IKE_SA_DELETED"
displaySystemErrorCode' ERROR_IPSEC_IKE_SA_REAPED                                                = Just "ERROR_IPSEC_IKE_SA_REAPED"
displaySystemErrorCode' ERROR_IPSEC_IKE_MM_ACQUIRE_DROP                                          = Just "ERROR_IPSEC_IKE_MM_ACQUIRE_DROP"
displaySystemErrorCode' ERROR_IPSEC_IKE_QM_ACQUIRE_DROP                                          = Just "ERROR_IPSEC_IKE_QM_ACQUIRE_DROP"
displaySystemErrorCode' ERROR_IPSEC_IKE_QUEUE_DROP_MM                                            = Just "ERROR_IPSEC_IKE_QUEUE_DROP_MM"
displaySystemErrorCode' ERROR_IPSEC_IKE_QUEUE_DROP_NO_MM                                         = Just "ERROR_IPSEC_IKE_QUEUE_DROP_NO_MM"
displaySystemErrorCode' ERROR_IPSEC_IKE_DROP_NO_RESPONSE                                         = Just "ERROR_IPSEC_IKE_DROP_NO_RESPONSE"
displaySystemErrorCode' ERROR_IPSEC_IKE_MM_DELAY_DROP                                            = Just "ERROR_IPSEC_IKE_MM_DELAY_DROP"
displaySystemErrorCode' ERROR_IPSEC_IKE_QM_DELAY_DROP                                            = Just "ERROR_IPSEC_IKE_QM_DELAY_DROP"
displaySystemErrorCode' ERROR_IPSEC_IKE_ERROR                                                    = Just "ERROR_IPSEC_IKE_ERROR"
displaySystemErrorCode' ERROR_IPSEC_IKE_CRL_FAILED                                               = Just "ERROR_IPSEC_IKE_CRL_FAILED"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_KEY_USAGE                                        = Just "ERROR_IPSEC_IKE_INVALID_KEY_USAGE"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_CERT_TYPE                                        = Just "ERROR_IPSEC_IKE_INVALID_CERT_TYPE"
displaySystemErrorCode' ERROR_IPSEC_IKE_NO_PRIVATE_KEY                                           = Just "ERROR_IPSEC_IKE_NO_PRIVATE_KEY"
displaySystemErrorCode' ERROR_IPSEC_IKE_SIMULTANEOUS_REKEY                                       = Just "ERROR_IPSEC_IKE_SIMULTANEOUS_REKEY"
displaySystemErrorCode' ERROR_IPSEC_IKE_DH_FAIL                                                  = Just "ERROR_IPSEC_IKE_DH_FAIL"
displaySystemErrorCode' ERROR_IPSEC_IKE_CRITICAL_PAYLOAD_NOT_RECOGNIZED                          = Just "ERROR_IPSEC_IKE_CRITICAL_PAYLOAD_NOT_RECOGNIZED"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_HEADER                                           = Just "ERROR_IPSEC_IKE_INVALID_HEADER"
displaySystemErrorCode' ERROR_IPSEC_IKE_NO_POLICY                                                = Just "ERROR_IPSEC_IKE_NO_POLICY"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_SIGNATURE                                        = Just "ERROR_IPSEC_IKE_INVALID_SIGNATURE"
displaySystemErrorCode' ERROR_IPSEC_IKE_KERBEROS_ERROR                                           = Just "ERROR_IPSEC_IKE_KERBEROS_ERROR"
displaySystemErrorCode' ERROR_IPSEC_IKE_NO_PUBLIC_KEY                                            = Just "ERROR_IPSEC_IKE_NO_PUBLIC_KEY"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR                                              = Just "ERROR_IPSEC_IKE_PROCESS_ERR"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_SA                                           = Just "ERROR_IPSEC_IKE_PROCESS_ERR_SA"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_PROP                                         = Just "ERROR_IPSEC_IKE_PROCESS_ERR_PROP"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_TRANS                                        = Just "ERROR_IPSEC_IKE_PROCESS_ERR_TRANS"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_KE                                           = Just "ERROR_IPSEC_IKE_PROCESS_ERR_KE"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_ID                                           = Just "ERROR_IPSEC_IKE_PROCESS_ERR_ID"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_CERT                                         = Just "ERROR_IPSEC_IKE_PROCESS_ERR_CERT"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_CERT_REQ                                     = Just "ERROR_IPSEC_IKE_PROCESS_ERR_CERT_REQ"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_HASH                                         = Just "ERROR_IPSEC_IKE_PROCESS_ERR_HASH"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_SIG                                          = Just "ERROR_IPSEC_IKE_PROCESS_ERR_SIG"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_NONCE                                        = Just "ERROR_IPSEC_IKE_PROCESS_ERR_NONCE"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_NOTIFY                                       = Just "ERROR_IPSEC_IKE_PROCESS_ERR_NOTIFY"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_DELETE                                       = Just "ERROR_IPSEC_IKE_PROCESS_ERR_DELETE"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_VENDOR                                       = Just "ERROR_IPSEC_IKE_PROCESS_ERR_VENDOR"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_PAYLOAD                                          = Just "ERROR_IPSEC_IKE_INVALID_PAYLOAD"
displaySystemErrorCode' ERROR_IPSEC_IKE_LOAD_SOFT_SA                                             = Just "ERROR_IPSEC_IKE_LOAD_SOFT_SA"
displaySystemErrorCode' ERROR_IPSEC_IKE_SOFT_SA_TORN_DOWN                                        = Just "ERROR_IPSEC_IKE_SOFT_SA_TORN_DOWN"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_COOKIE                                           = Just "ERROR_IPSEC_IKE_INVALID_COOKIE"
displaySystemErrorCode' ERROR_IPSEC_IKE_NO_PEER_CERT                                             = Just "ERROR_IPSEC_IKE_NO_PEER_CERT"
displaySystemErrorCode' ERROR_IPSEC_IKE_PEER_CRL_FAILED                                          = Just "ERROR_IPSEC_IKE_PEER_CRL_FAILED"
displaySystemErrorCode' ERROR_IPSEC_IKE_POLICY_CHANGE                                            = Just "ERROR_IPSEC_IKE_POLICY_CHANGE"
displaySystemErrorCode' ERROR_IPSEC_IKE_NO_MM_POLICY                                             = Just "ERROR_IPSEC_IKE_NO_MM_POLICY"
displaySystemErrorCode' ERROR_IPSEC_IKE_NOTCBPRIV                                                = Just "ERROR_IPSEC_IKE_NOTCBPRIV"
displaySystemErrorCode' ERROR_IPSEC_IKE_SECLOADFAIL                                              = Just "ERROR_IPSEC_IKE_SECLOADFAIL"
displaySystemErrorCode' ERROR_IPSEC_IKE_FAILSSPINIT                                              = Just "ERROR_IPSEC_IKE_FAILSSPINIT"
displaySystemErrorCode' ERROR_IPSEC_IKE_FAILQUERYSSP                                             = Just "ERROR_IPSEC_IKE_FAILQUERYSSP"
displaySystemErrorCode' ERROR_IPSEC_IKE_SRVACQFAIL                                               = Just "ERROR_IPSEC_IKE_SRVACQFAIL"
displaySystemErrorCode' ERROR_IPSEC_IKE_SRVQUERYCRED                                             = Just "ERROR_IPSEC_IKE_SRVQUERYCRED"
displaySystemErrorCode' ERROR_IPSEC_IKE_GETSPIFAIL                                               = Just "ERROR_IPSEC_IKE_GETSPIFAIL"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_FILTER                                           = Just "ERROR_IPSEC_IKE_INVALID_FILTER"
displaySystemErrorCode' ERROR_IPSEC_IKE_OUT_OF_MEMORY                                            = Just "ERROR_IPSEC_IKE_OUT_OF_MEMORY"
displaySystemErrorCode' ERROR_IPSEC_IKE_ADD_UPDATE_KEY_FAILED                                    = Just "ERROR_IPSEC_IKE_ADD_UPDATE_KEY_FAILED"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_POLICY                                           = Just "ERROR_IPSEC_IKE_INVALID_POLICY"
displaySystemErrorCode' ERROR_IPSEC_IKE_UNKNOWN_DOI                                              = Just "ERROR_IPSEC_IKE_UNKNOWN_DOI"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_SITUATION                                        = Just "ERROR_IPSEC_IKE_INVALID_SITUATION"
displaySystemErrorCode' ERROR_IPSEC_IKE_DH_FAILURE                                               = Just "ERROR_IPSEC_IKE_DH_FAILURE"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_GROUP                                            = Just "ERROR_IPSEC_IKE_INVALID_GROUP"
displaySystemErrorCode' ERROR_IPSEC_IKE_ENCRYPT                                                  = Just "ERROR_IPSEC_IKE_ENCRYPT"
displaySystemErrorCode' ERROR_IPSEC_IKE_DECRYPT                                                  = Just "ERROR_IPSEC_IKE_DECRYPT"
displaySystemErrorCode' ERROR_IPSEC_IKE_POLICY_MATCH                                             = Just "ERROR_IPSEC_IKE_POLICY_MATCH"
displaySystemErrorCode' ERROR_IPSEC_IKE_UNSUPPORTED_ID                                           = Just "ERROR_IPSEC_IKE_UNSUPPORTED_ID"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_HASH                                             = Just "ERROR_IPSEC_IKE_INVALID_HASH"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_HASH_ALG                                         = Just "ERROR_IPSEC_IKE_INVALID_HASH_ALG"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_HASH_SIZE                                        = Just "ERROR_IPSEC_IKE_INVALID_HASH_SIZE"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_ENCRYPT_ALG                                      = Just "ERROR_IPSEC_IKE_INVALID_ENCRYPT_ALG"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_AUTH_ALG                                         = Just "ERROR_IPSEC_IKE_INVALID_AUTH_ALG"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_SIG                                              = Just "ERROR_IPSEC_IKE_INVALID_SIG"
displaySystemErrorCode' ERROR_IPSEC_IKE_LOAD_FAILED                                              = Just "ERROR_IPSEC_IKE_LOAD_FAILED"
displaySystemErrorCode' ERROR_IPSEC_IKE_RPC_DELETE                                               = Just "ERROR_IPSEC_IKE_RPC_DELETE"
displaySystemErrorCode' ERROR_IPSEC_IKE_BENIGN_REINIT                                            = Just "ERROR_IPSEC_IKE_BENIGN_REINIT"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_RESPONDER_LIFETIME_NOTIFY                        = Just "ERROR_IPSEC_IKE_INVALID_RESPONDER_LIFETIME_NOTIFY"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_MAJOR_VERSION                                    = Just "ERROR_IPSEC_IKE_INVALID_MAJOR_VERSION"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_CERT_KEYLEN                                      = Just "ERROR_IPSEC_IKE_INVALID_CERT_KEYLEN"
displaySystemErrorCode' ERROR_IPSEC_IKE_MM_LIMIT                                                 = Just "ERROR_IPSEC_IKE_MM_LIMIT"
displaySystemErrorCode' ERROR_IPSEC_IKE_NEGOTIATION_DISABLED                                     = Just "ERROR_IPSEC_IKE_NEGOTIATION_DISABLED"
displaySystemErrorCode' ERROR_IPSEC_IKE_QM_LIMIT                                                 = Just "ERROR_IPSEC_IKE_QM_LIMIT"
displaySystemErrorCode' ERROR_IPSEC_IKE_MM_EXPIRED                                               = Just "ERROR_IPSEC_IKE_MM_EXPIRED"
displaySystemErrorCode' ERROR_IPSEC_IKE_PEER_MM_ASSUMED_INVALID                                  = Just "ERROR_IPSEC_IKE_PEER_MM_ASSUMED_INVALID"
displaySystemErrorCode' ERROR_IPSEC_IKE_CERT_CHAIN_POLICY_MISMATCH                               = Just "ERROR_IPSEC_IKE_CERT_CHAIN_POLICY_MISMATCH"
displaySystemErrorCode' ERROR_IPSEC_IKE_UNEXPECTED_MESSAGE_ID                                    = Just "ERROR_IPSEC_IKE_UNEXPECTED_MESSAGE_ID"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_AUTH_PAYLOAD                                     = Just "ERROR_IPSEC_IKE_INVALID_AUTH_PAYLOAD"
displaySystemErrorCode' ERROR_IPSEC_IKE_DOS_COOKIE_SENT                                          = Just "ERROR_IPSEC_IKE_DOS_COOKIE_SENT"
displaySystemErrorCode' ERROR_IPSEC_IKE_SHUTTING_DOWN                                            = Just "ERROR_IPSEC_IKE_SHUTTING_DOWN"
displaySystemErrorCode' ERROR_IPSEC_IKE_CGA_AUTH_FAILED                                          = Just "ERROR_IPSEC_IKE_CGA_AUTH_FAILED"
displaySystemErrorCode' ERROR_IPSEC_IKE_PROCESS_ERR_NATOA                                        = Just "ERROR_IPSEC_IKE_PROCESS_ERR_NATOA"
displaySystemErrorCode' ERROR_IPSEC_IKE_INVALID_MM_FOR_QM                                        = Just "ERROR_IPSEC_IKE_INVALID_MM_FOR_QM"
displaySystemErrorCode' ERROR_IPSEC_IKE_QM_EXPIRED                                               = Just "ERROR_IPSEC_IKE_QM_EXPIRED"
displaySystemErrorCode' ERROR_IPSEC_IKE_TOO_MANY_FILTERS                                         = Just "ERROR_IPSEC_IKE_TOO_MANY_FILTERS"
displaySystemErrorCode' ERROR_IPSEC_IKE_NEG_STATUS_END                                           = Just "ERROR_IPSEC_IKE_NEG_STATUS_END"
displaySystemErrorCode' ERROR_IPSEC_IKE_KILL_DUMMY_NAP_TUNNEL                                    = Just "ERROR_IPSEC_IKE_KILL_DUMMY_NAP_TUNNEL"
displaySystemErrorCode' ERROR_IPSEC_IKE_INNER_IP_ASSIGNMENT_FAILURE                              = Just "ERROR_IPSEC_IKE_INNER_IP_ASSIGNMENT_FAILURE"
displaySystemErrorCode' ERROR_IPSEC_IKE_REQUIRE_CP_PAYLOAD_MISSING                               = Just "ERROR_IPSEC_IKE_REQUIRE_CP_PAYLOAD_MISSING"
displaySystemErrorCode' ERROR_IPSEC_KEY_MODULE_IMPERSONATION_NEGOTIATION_PENDING                 = Just "ERROR_IPSEC_KEY_MODULE_IMPERSONATION_NEGOTIATION_PENDING"
displaySystemErrorCode' ERROR_IPSEC_IKE_COEXISTENCE_SUPPRESS                                     = Just "ERROR_IPSEC_IKE_COEXISTENCE_SUPPRESS"
displaySystemErrorCode' ERROR_IPSEC_IKE_RATELIMIT_DROP                                           = Just "ERROR_IPSEC_IKE_RATELIMIT_DROP"
displaySystemErrorCode' ERROR_IPSEC_IKE_PEER_DOESNT_SUPPORT_MOBIKE                               = Just "ERROR_IPSEC_IKE_PEER_DOESNT_SUPPORT_MOBIKE"
displaySystemErrorCode' ERROR_IPSEC_IKE_AUTHORIZATION_FAILURE                                    = Just "ERROR_IPSEC_IKE_AUTHORIZATION_FAILURE"
displaySystemErrorCode' ERROR_IPSEC_IKE_STRONG_CRED_AUTHORIZATION_FAILURE                        = Just "ERROR_IPSEC_IKE_STRONG_CRED_AUTHORIZATION_FAILURE"
displaySystemErrorCode' ERROR_IPSEC_IKE_AUTHORIZATION_FAILURE_WITH_OPTIONAL_RETRY                = Just "ERROR_IPSEC_IKE_AUTHORIZATION_FAILURE_WITH_OPTIONAL_RETRY"
displaySystemErrorCode' ERROR_IPSEC_IKE_STRONG_CRED_AUTHORIZATION_AND_CERTMAP_FAILURE            = Just "ERROR_IPSEC_IKE_STRONG_CRED_AUTHORIZATION_AND_CERTMAP_FAILURE"
displaySystemErrorCode' ERROR_IPSEC_IKE_NEG_STATUS_EXTENDED_END                                  = Just "ERROR_IPSEC_IKE_NEG_STATUS_EXTENDED_END"
displaySystemErrorCode' ERROR_IPSEC_BAD_SPI                                                      = Just "ERROR_IPSEC_BAD_SPI"
displaySystemErrorCode' ERROR_IPSEC_SA_LIFETIME_EXPIRED                                          = Just "ERROR_IPSEC_SA_LIFETIME_EXPIRED"
displaySystemErrorCode' ERROR_IPSEC_WRONG_SA                                                     = Just "ERROR_IPSEC_WRONG_SA"
displaySystemErrorCode' ERROR_IPSEC_REPLAY_CHECK_FAILED                                          = Just "ERROR_IPSEC_REPLAY_CHECK_FAILED"
displaySystemErrorCode' ERROR_IPSEC_INVALID_PACKET                                               = Just "ERROR_IPSEC_INVALID_PACKET"
displaySystemErrorCode' ERROR_IPSEC_INTEGRITY_CHECK_FAILED                                       = Just "ERROR_IPSEC_INTEGRITY_CHECK_FAILED"
displaySystemErrorCode' ERROR_IPSEC_CLEAR_TEXT_DROP                                              = Just "ERROR_IPSEC_CLEAR_TEXT_DROP"
displaySystemErrorCode' ERROR_IPSEC_AUTH_FIREWALL_DROP                                           = Just "ERROR_IPSEC_AUTH_FIREWALL_DROP"
displaySystemErrorCode' ERROR_IPSEC_THROTTLE_DROP                                                = Just "ERROR_IPSEC_THROTTLE_DROP"
displaySystemErrorCode' ERROR_IPSEC_DOSP_BLOCK                                                   = Just "ERROR_IPSEC_DOSP_BLOCK"
displaySystemErrorCode' ERROR_IPSEC_DOSP_RECEIVED_MULTICAST                                      = Just "ERROR_IPSEC_DOSP_RECEIVED_MULTICAST"
displaySystemErrorCode' ERROR_IPSEC_DOSP_INVALID_PACKET                                          = Just "ERROR_IPSEC_DOSP_INVALID_PACKET"
displaySystemErrorCode' ERROR_IPSEC_DOSP_STATE_LOOKUP_FAILED                                     = Just "ERROR_IPSEC_DOSP_STATE_LOOKUP_FAILED"
displaySystemErrorCode' ERROR_IPSEC_DOSP_MAX_ENTRIES                                             = Just "ERROR_IPSEC_DOSP_MAX_ENTRIES"
displaySystemErrorCode' ERROR_IPSEC_DOSP_KEYMOD_NOT_ALLOWED                                      = Just "ERROR_IPSEC_DOSP_KEYMOD_NOT_ALLOWED"
displaySystemErrorCode' ERROR_IPSEC_DOSP_NOT_INSTALLED                                           = Just "ERROR_IPSEC_DOSP_NOT_INSTALLED"
displaySystemErrorCode' ERROR_IPSEC_DOSP_MAX_PER_IP_RATELIMIT_QUEUES                             = Just "ERROR_IPSEC_DOSP_MAX_PER_IP_RATELIMIT_QUEUES"
displaySystemErrorCode' ERROR_SXS_SECTION_NOT_FOUND                                              = Just "ERROR_SXS_SECTION_NOT_FOUND"
displaySystemErrorCode' ERROR_SXS_CANT_GEN_ACTCTX                                                = Just "ERROR_SXS_CANT_GEN_ACTCTX"
displaySystemErrorCode' ERROR_SXS_INVALID_ACTCTXDATA_FORMAT                                      = Just "ERROR_SXS_INVALID_ACTCTXDATA_FORMAT"
displaySystemErrorCode' ERROR_SXS_ASSEMBLY_NOT_FOUND                                             = Just "ERROR_SXS_ASSEMBLY_NOT_FOUND"
displaySystemErrorCode' ERROR_SXS_MANIFEST_FORMAT_ERROR                                          = Just "ERROR_SXS_MANIFEST_FORMAT_ERROR"
displaySystemErrorCode' ERROR_SXS_MANIFEST_PARSE_ERROR                                           = Just "ERROR_SXS_MANIFEST_PARSE_ERROR"
displaySystemErrorCode' ERROR_SXS_ACTIVATION_CONTEXT_DISABLED                                    = Just "ERROR_SXS_ACTIVATION_CONTEXT_DISABLED"
displaySystemErrorCode' ERROR_SXS_KEY_NOT_FOUND                                                  = Just "ERROR_SXS_KEY_NOT_FOUND"
displaySystemErrorCode' ERROR_SXS_VERSION_CONFLICT                                               = Just "ERROR_SXS_VERSION_CONFLICT"
displaySystemErrorCode' ERROR_SXS_WRONG_SECTION_TYPE                                             = Just "ERROR_SXS_WRONG_SECTION_TYPE"
displaySystemErrorCode' ERROR_SXS_THREAD_QUERIES_DISABLED                                        = Just "ERROR_SXS_THREAD_QUERIES_DISABLED"
displaySystemErrorCode' ERROR_SXS_PROCESS_DEFAULT_ALREADY_SET                                    = Just "ERROR_SXS_PROCESS_DEFAULT_ALREADY_SET"
displaySystemErrorCode' ERROR_SXS_UNKNOWN_ENCODING_GROUP                                         = Just "ERROR_SXS_UNKNOWN_ENCODING_GROUP"
displaySystemErrorCode' ERROR_SXS_UNKNOWN_ENCODING                                               = Just "ERROR_SXS_UNKNOWN_ENCODING"
displaySystemErrorCode' ERROR_SXS_INVALID_XML_NAMESPACE_URI                                      = Just "ERROR_SXS_INVALID_XML_NAMESPACE_URI"
displaySystemErrorCode' ERROR_SXS_ROOT_MANIFEST_DEPENDENCY_NOT_INSTALLED                         = Just "ERROR_SXS_ROOT_MANIFEST_DEPENDENCY_NOT_INSTALLED"
displaySystemErrorCode' ERROR_SXS_LEAF_MANIFEST_DEPENDENCY_NOT_INSTALLED                         = Just "ERROR_SXS_LEAF_MANIFEST_DEPENDENCY_NOT_INSTALLED"
displaySystemErrorCode' ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE                            = Just "ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE"
displaySystemErrorCode' ERROR_SXS_MANIFEST_MISSING_REQUIRED_DEFAULT_NAMESPACE                    = Just "ERROR_SXS_MANIFEST_MISSING_REQUIRED_DEFAULT_NAMESPACE"
displaySystemErrorCode' ERROR_SXS_MANIFEST_INVALID_REQUIRED_DEFAULT_NAMESPACE                    = Just "ERROR_SXS_MANIFEST_INVALID_REQUIRED_DEFAULT_NAMESPACE"
displaySystemErrorCode' ERROR_SXS_PRIVATE_MANIFEST_CROSS_PATH_WITH_REPARSE_POINT                 = Just "ERROR_SXS_PRIVATE_MANIFEST_CROSS_PATH_WITH_REPARSE_POINT"
displaySystemErrorCode' ERROR_SXS_DUPLICATE_DLL_NAME                                             = Just "ERROR_SXS_DUPLICATE_DLL_NAME"
displaySystemErrorCode' ERROR_SXS_DUPLICATE_WINDOWCLASS_NAME                                     = Just "ERROR_SXS_DUPLICATE_WINDOWCLASS_NAME"
displaySystemErrorCode' ERROR_SXS_DUPLICATE_CLSID                                                = Just "ERROR_SXS_DUPLICATE_CLSID"
displaySystemErrorCode' ERROR_SXS_DUPLICATE_IID                                                  = Just "ERROR_SXS_DUPLICATE_IID"
displaySystemErrorCode' ERROR_SXS_DUPLICATE_TLBID                                                = Just "ERROR_SXS_DUPLICATE_TLBID"
displaySystemErrorCode' ERROR_SXS_DUPLICATE_PROGID                                               = Just "ERROR_SXS_DUPLICATE_PROGID"
displaySystemErrorCode' ERROR_SXS_DUPLICATE_ASSEMBLY_NAME                                        = Just "ERROR_SXS_DUPLICATE_ASSEMBLY_NAME"
displaySystemErrorCode' ERROR_SXS_FILE_HASH_MISMATCH                                             = Just "ERROR_SXS_FILE_HASH_MISMATCH"
displaySystemErrorCode' ERROR_SXS_POLICY_PARSE_ERROR                                             = Just "ERROR_SXS_POLICY_PARSE_ERROR"
displaySystemErrorCode' ERROR_SXS_XML_E_MISSINGQUOTE                                             = Just "ERROR_SXS_XML_E_MISSINGQUOTE"
displaySystemErrorCode' ERROR_SXS_XML_E_COMMENTSYNTAX                                            = Just "ERROR_SXS_XML_E_COMMENTSYNTAX"
displaySystemErrorCode' ERROR_SXS_XML_E_BADSTARTNAMECHAR                                         = Just "ERROR_SXS_XML_E_BADSTARTNAMECHAR"
displaySystemErrorCode' ERROR_SXS_XML_E_BADNAMECHAR                                              = Just "ERROR_SXS_XML_E_BADNAMECHAR"
displaySystemErrorCode' ERROR_SXS_XML_E_BADCHARINSTRING                                          = Just "ERROR_SXS_XML_E_BADCHARINSTRING"
displaySystemErrorCode' ERROR_SXS_XML_E_XMLDECLSYNTAX                                            = Just "ERROR_SXS_XML_E_XMLDECLSYNTAX"
displaySystemErrorCode' ERROR_SXS_XML_E_BADCHARDATA                                              = Just "ERROR_SXS_XML_E_BADCHARDATA"
displaySystemErrorCode' ERROR_SXS_XML_E_MISSINGWHITESPACE                                        = Just "ERROR_SXS_XML_E_MISSINGWHITESPACE"
displaySystemErrorCode' ERROR_SXS_XML_E_EXPECTINGTAGEND                                          = Just "ERROR_SXS_XML_E_EXPECTINGTAGEND"
displaySystemErrorCode' ERROR_SXS_XML_E_MISSINGSEMICOLON                                         = Just "ERROR_SXS_XML_E_MISSINGSEMICOLON"
displaySystemErrorCode' ERROR_SXS_XML_E_UNBALANCEDPAREN                                          = Just "ERROR_SXS_XML_E_UNBALANCEDPAREN"
displaySystemErrorCode' ERROR_SXS_XML_E_INTERNALERROR                                            = Just "ERROR_SXS_XML_E_INTERNALERROR"
displaySystemErrorCode' ERROR_SXS_XML_E_UNEXPECTED_WHITESPACE                                    = Just "ERROR_SXS_XML_E_UNEXPECTED_WHITESPACE"
displaySystemErrorCode' ERROR_SXS_XML_E_INCOMPLETE_ENCODING                                      = Just "ERROR_SXS_XML_E_INCOMPLETE_ENCODING"
displaySystemErrorCode' ERROR_SXS_XML_E_MISSING_PAREN                                            = Just "ERROR_SXS_XML_E_MISSING_PAREN"
displaySystemErrorCode' ERROR_SXS_XML_E_EXPECTINGCLOSEQUOTE                                      = Just "ERROR_SXS_XML_E_EXPECTINGCLOSEQUOTE"
displaySystemErrorCode' ERROR_SXS_XML_E_MULTIPLE_COLONS                                          = Just "ERROR_SXS_XML_E_MULTIPLE_COLONS"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALID_DECIMAL                                          = Just "ERROR_SXS_XML_E_INVALID_DECIMAL"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALID_HEXIDECIMAL                                      = Just "ERROR_SXS_XML_E_INVALID_HEXIDECIMAL"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALID_UNICODE                                          = Just "ERROR_SXS_XML_E_INVALID_UNICODE"
displaySystemErrorCode' ERROR_SXS_XML_E_WHITESPACEORQUESTIONMARK                                 = Just "ERROR_SXS_XML_E_WHITESPACEORQUESTIONMARK"
displaySystemErrorCode' ERROR_SXS_XML_E_UNEXPECTEDENDTAG                                         = Just "ERROR_SXS_XML_E_UNEXPECTEDENDTAG"
displaySystemErrorCode' ERROR_SXS_XML_E_UNCLOSEDTAG                                              = Just "ERROR_SXS_XML_E_UNCLOSEDTAG"
displaySystemErrorCode' ERROR_SXS_XML_E_DUPLICATEATTRIBUTE                                       = Just "ERROR_SXS_XML_E_DUPLICATEATTRIBUTE"
displaySystemErrorCode' ERROR_SXS_XML_E_MULTIPLEROOTS                                            = Just "ERROR_SXS_XML_E_MULTIPLEROOTS"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALIDATROOTLEVEL                                       = Just "ERROR_SXS_XML_E_INVALIDATROOTLEVEL"
displaySystemErrorCode' ERROR_SXS_XML_E_BADXMLDECL                                               = Just "ERROR_SXS_XML_E_BADXMLDECL"
displaySystemErrorCode' ERROR_SXS_XML_E_MISSINGROOT                                              = Just "ERROR_SXS_XML_E_MISSINGROOT"
displaySystemErrorCode' ERROR_SXS_XML_E_UNEXPECTEDEOF                                            = Just "ERROR_SXS_XML_E_UNEXPECTEDEOF"
displaySystemErrorCode' ERROR_SXS_XML_E_BADPEREFINSUBSET                                         = Just "ERROR_SXS_XML_E_BADPEREFINSUBSET"
displaySystemErrorCode' ERROR_SXS_XML_E_UNCLOSEDSTARTTAG                                         = Just "ERROR_SXS_XML_E_UNCLOSEDSTARTTAG"
displaySystemErrorCode' ERROR_SXS_XML_E_UNCLOSEDENDTAG                                           = Just "ERROR_SXS_XML_E_UNCLOSEDENDTAG"
displaySystemErrorCode' ERROR_SXS_XML_E_UNCLOSEDSTRING                                           = Just "ERROR_SXS_XML_E_UNCLOSEDSTRING"
displaySystemErrorCode' ERROR_SXS_XML_E_UNCLOSEDCOMMENT                                          = Just "ERROR_SXS_XML_E_UNCLOSEDCOMMENT"
displaySystemErrorCode' ERROR_SXS_XML_E_UNCLOSEDDECL                                             = Just "ERROR_SXS_XML_E_UNCLOSEDDECL"
displaySystemErrorCode' ERROR_SXS_XML_E_UNCLOSEDCDATA                                            = Just "ERROR_SXS_XML_E_UNCLOSEDCDATA"
displaySystemErrorCode' ERROR_SXS_XML_E_RESERVEDNAMESPACE                                        = Just "ERROR_SXS_XML_E_RESERVEDNAMESPACE"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALIDENCODING                                          = Just "ERROR_SXS_XML_E_INVALIDENCODING"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALIDSWITCH                                            = Just "ERROR_SXS_XML_E_INVALIDSWITCH"
displaySystemErrorCode' ERROR_SXS_XML_E_BADXMLCASE                                               = Just "ERROR_SXS_XML_E_BADXMLCASE"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALID_STANDALONE                                       = Just "ERROR_SXS_XML_E_INVALID_STANDALONE"
displaySystemErrorCode' ERROR_SXS_XML_E_UNEXPECTED_STANDALONE                                    = Just "ERROR_SXS_XML_E_UNEXPECTED_STANDALONE"
displaySystemErrorCode' ERROR_SXS_XML_E_INVALID_VERSION                                          = Just "ERROR_SXS_XML_E_INVALID_VERSION"
displaySystemErrorCode' ERROR_SXS_XML_E_MISSINGEQUALS                                            = Just "ERROR_SXS_XML_E_MISSINGEQUALS"
displaySystemErrorCode' ERROR_SXS_PROTECTION_RECOVERY_FAILED                                     = Just "ERROR_SXS_PROTECTION_RECOVERY_FAILED"
displaySystemErrorCode' ERROR_SXS_PROTECTION_PUBLIC_KEY_TOO_SHORT                                = Just "ERROR_SXS_PROTECTION_PUBLIC_KEY_TOO_SHORT"
displaySystemErrorCode' ERROR_SXS_PROTECTION_CATALOG_NOT_VALID                                   = Just "ERROR_SXS_PROTECTION_CATALOG_NOT_VALID"
displaySystemErrorCode' ERROR_SXS_UNTRANSLATABLE_HRESULT                                         = Just "ERROR_SXS_UNTRANSLATABLE_HRESULT"
displaySystemErrorCode' ERROR_SXS_PROTECTION_CATALOG_FILE_MISSING                                = Just "ERROR_SXS_PROTECTION_CATALOG_FILE_MISSING"
displaySystemErrorCode' ERROR_SXS_MISSING_ASSEMBLY_IDENTITY_ATTRIBUTE                            = Just "ERROR_SXS_MISSING_ASSEMBLY_IDENTITY_ATTRIBUTE"
displaySystemErrorCode' ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE_NAME                       = Just "ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE_NAME"
displaySystemErrorCode' ERROR_SXS_ASSEMBLY_MISSING                                               = Just "ERROR_SXS_ASSEMBLY_MISSING"
displaySystemErrorCode' ERROR_SXS_CORRUPT_ACTIVATION_STACK                                       = Just "ERROR_SXS_CORRUPT_ACTIVATION_STACK"
displaySystemErrorCode' ERROR_SXS_CORRUPTION                                                     = Just "ERROR_SXS_CORRUPTION"
displaySystemErrorCode' ERROR_SXS_EARLY_DEACTIVATION                                             = Just "ERROR_SXS_EARLY_DEACTIVATION"
displaySystemErrorCode' ERROR_SXS_INVALID_DEACTIVATION                                           = Just "ERROR_SXS_INVALID_DEACTIVATION"
displaySystemErrorCode' ERROR_SXS_MULTIPLE_DEACTIVATION                                          = Just "ERROR_SXS_MULTIPLE_DEACTIVATION"
displaySystemErrorCode' ERROR_SXS_PROCESS_TERMINATION_REQUESTED                                  = Just "ERROR_SXS_PROCESS_TERMINATION_REQUESTED"
displaySystemErrorCode' ERROR_SXS_RELEASE_ACTIVATION_CONTEXT                                     = Just "ERROR_SXS_RELEASE_ACTIVATION_CONTEXT"
displaySystemErrorCode' ERROR_SXS_SYSTEM_DEFAULT_ACTIVATION_CONTEXT_EMPTY                        = Just "ERROR_SXS_SYSTEM_DEFAULT_ACTIVATION_CONTEXT_EMPTY"
displaySystemErrorCode' ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_VALUE                               = Just "ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_VALUE"
displaySystemErrorCode' ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_NAME                                = Just "ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_NAME"
displaySystemErrorCode' ERROR_SXS_IDENTITY_DUPLICATE_ATTRIBUTE                                   = Just "ERROR_SXS_IDENTITY_DUPLICATE_ATTRIBUTE"
displaySystemErrorCode' ERROR_SXS_IDENTITY_PARSE_ERROR                                           = Just "ERROR_SXS_IDENTITY_PARSE_ERROR"
displaySystemErrorCode' ERROR_MALFORMED_SUBSTITUTION_STRING                                      = Just "ERROR_MALFORMED_SUBSTITUTION_STRING"
displaySystemErrorCode' ERROR_SXS_INCORRECT_PUBLIC_KEY_TOKEN                                     = Just "ERROR_SXS_INCORRECT_PUBLIC_KEY_TOKEN"
displaySystemErrorCode' ERROR_UNMAPPED_SUBSTITUTION_STRING                                       = Just "ERROR_UNMAPPED_SUBSTITUTION_STRING"
displaySystemErrorCode' ERROR_SXS_ASSEMBLY_NOT_LOCKED                                            = Just "ERROR_SXS_ASSEMBLY_NOT_LOCKED"
displaySystemErrorCode' ERROR_SXS_COMPONENT_STORE_CORRUPT                                        = Just "ERROR_SXS_COMPONENT_STORE_CORRUPT"
displaySystemErrorCode' ERROR_ADVANCED_INSTALLER_FAILED                                          = Just "ERROR_ADVANCED_INSTALLER_FAILED"
displaySystemErrorCode' ERROR_XML_ENCODING_MISMATCH                                              = Just "ERROR_XML_ENCODING_MISMATCH"
displaySystemErrorCode' ERROR_SXS_MANIFEST_IDENTITY_SAME_BUT_CONTENTS_DIFFERENT                  = Just "ERROR_SXS_MANIFEST_IDENTITY_SAME_BUT_CONTENTS_DIFFERENT"
displaySystemErrorCode' ERROR_SXS_IDENTITIES_DIFFERENT                                           = Just "ERROR_SXS_IDENTITIES_DIFFERENT"
displaySystemErrorCode' ERROR_SXS_ASSEMBLY_IS_NOT_A_DEPLOYMENT                                   = Just "ERROR_SXS_ASSEMBLY_IS_NOT_A_DEPLOYMENT"
displaySystemErrorCode' ERROR_SXS_FILE_NOT_PART_OF_ASSEMBLY                                      = Just "ERROR_SXS_FILE_NOT_PART_OF_ASSEMBLY"
displaySystemErrorCode' ERROR_SXS_MANIFEST_TOO_BIG                                               = Just "ERROR_SXS_MANIFEST_TOO_BIG"
displaySystemErrorCode' ERROR_SXS_SETTING_NOT_REGISTERED                                         = Just "ERROR_SXS_SETTING_NOT_REGISTERED"
displaySystemErrorCode' ERROR_SXS_TRANSACTION_CLOSURE_INCOMPLETE                                 = Just "ERROR_SXS_TRANSACTION_CLOSURE_INCOMPLETE"
displaySystemErrorCode' ERROR_SMI_PRIMITIVE_INSTALLER_FAILED                                     = Just "ERROR_SMI_PRIMITIVE_INSTALLER_FAILED"
displaySystemErrorCode' ERROR_GENERIC_COMMAND_FAILED                                             = Just "ERROR_GENERIC_COMMAND_FAILED"
displaySystemErrorCode' ERROR_SXS_FILE_HASH_MISSING                                              = Just "ERROR_SXS_FILE_HASH_MISSING"
displaySystemErrorCode' ERROR_EVT_INVALID_CHANNEL_PATH                                           = Just "ERROR_EVT_INVALID_CHANNEL_PATH"
displaySystemErrorCode' ERROR_EVT_INVALID_QUERY                                                  = Just "ERROR_EVT_INVALID_QUERY"
displaySystemErrorCode' ERROR_EVT_PUBLISHER_METADATA_NOT_FOUND                                   = Just "ERROR_EVT_PUBLISHER_METADATA_NOT_FOUND"
displaySystemErrorCode' ERROR_EVT_EVENT_TEMPLATE_NOT_FOUND                                       = Just "ERROR_EVT_EVENT_TEMPLATE_NOT_FOUND"
displaySystemErrorCode' ERROR_EVT_INVALID_PUBLISHER_NAME                                         = Just "ERROR_EVT_INVALID_PUBLISHER_NAME"
displaySystemErrorCode' ERROR_EVT_INVALID_EVENT_DATA                                             = Just "ERROR_EVT_INVALID_EVENT_DATA"
displaySystemErrorCode' ERROR_EVT_CHANNEL_NOT_FOUND                                              = Just "ERROR_EVT_CHANNEL_NOT_FOUND"
displaySystemErrorCode' ERROR_EVT_MALFORMED_XML_TEXT                                             = Just "ERROR_EVT_MALFORMED_XML_TEXT"
displaySystemErrorCode' ERROR_EVT_SUBSCRIPTION_TO_DIRECT_CHANNEL                                 = Just "ERROR_EVT_SUBSCRIPTION_TO_DIRECT_CHANNEL"
displaySystemErrorCode' ERROR_EVT_CONFIGURATION_ERROR                                            = Just "ERROR_EVT_CONFIGURATION_ERROR"
displaySystemErrorCode' ERROR_EVT_QUERY_RESULT_STALE                                             = Just "ERROR_EVT_QUERY_RESULT_STALE"
displaySystemErrorCode' ERROR_EVT_QUERY_RESULT_INVALID_POSITION                                  = Just "ERROR_EVT_QUERY_RESULT_INVALID_POSITION"
displaySystemErrorCode' ERROR_EVT_NON_VALIDATING_MSXML                                           = Just "ERROR_EVT_NON_VALIDATING_MSXML"
displaySystemErrorCode' ERROR_EVT_FILTER_ALREADYSCOPED                                           = Just "ERROR_EVT_FILTER_ALREADYSCOPED"
displaySystemErrorCode' ERROR_EVT_FILTER_NOTELTSET                                               = Just "ERROR_EVT_FILTER_NOTELTSET"
displaySystemErrorCode' ERROR_EVT_FILTER_INVARG                                                  = Just "ERROR_EVT_FILTER_INVARG"
displaySystemErrorCode' ERROR_EVT_FILTER_INVTEST                                                 = Just "ERROR_EVT_FILTER_INVTEST"
displaySystemErrorCode' ERROR_EVT_FILTER_INVTYPE                                                 = Just "ERROR_EVT_FILTER_INVTYPE"
displaySystemErrorCode' ERROR_EVT_FILTER_PARSEERR                                                = Just "ERROR_EVT_FILTER_PARSEERR"
displaySystemErrorCode' ERROR_EVT_FILTER_UNSUPPORTEDOP                                           = Just "ERROR_EVT_FILTER_UNSUPPORTEDOP"
displaySystemErrorCode' ERROR_EVT_FILTER_UNEXPECTEDTOKEN                                         = Just "ERROR_EVT_FILTER_UNEXPECTEDTOKEN"
displaySystemErrorCode' ERROR_EVT_INVALID_OPERATION_OVER_ENABLED_DIRECT_CHANNEL                  = Just "ERROR_EVT_INVALID_OPERATION_OVER_ENABLED_DIRECT_CHANNEL"
displaySystemErrorCode' ERROR_EVT_INVALID_CHANNEL_PROPERTY_VALUE                                 = Just "ERROR_EVT_INVALID_CHANNEL_PROPERTY_VALUE"
displaySystemErrorCode' ERROR_EVT_INVALID_PUBLISHER_PROPERTY_VALUE                               = Just "ERROR_EVT_INVALID_PUBLISHER_PROPERTY_VALUE"
displaySystemErrorCode' ERROR_EVT_CHANNEL_CANNOT_ACTIVATE                                        = Just "ERROR_EVT_CHANNEL_CANNOT_ACTIVATE"
displaySystemErrorCode' ERROR_EVT_FILTER_TOO_COMPLEX                                             = Just "ERROR_EVT_FILTER_TOO_COMPLEX"
displaySystemErrorCode' ERROR_EVT_MESSAGE_NOT_FOUND                                              = Just "ERROR_EVT_MESSAGE_NOT_FOUND"
displaySystemErrorCode' ERROR_EVT_MESSAGE_ID_NOT_FOUND                                           = Just "ERROR_EVT_MESSAGE_ID_NOT_FOUND"
displaySystemErrorCode' ERROR_EVT_UNRESOLVED_VALUE_INSERT                                        = Just "ERROR_EVT_UNRESOLVED_VALUE_INSERT"
displaySystemErrorCode' ERROR_EVT_UNRESOLVED_PARAMETER_INSERT                                    = Just "ERROR_EVT_UNRESOLVED_PARAMETER_INSERT"
displaySystemErrorCode' ERROR_EVT_MAX_INSERTS_REACHED                                            = Just "ERROR_EVT_MAX_INSERTS_REACHED"
displaySystemErrorCode' ERROR_EVT_EVENT_DEFINITION_NOT_FOUND                                     = Just "ERROR_EVT_EVENT_DEFINITION_NOT_FOUND"
displaySystemErrorCode' ERROR_EVT_MESSAGE_LOCALE_NOT_FOUND                                       = Just "ERROR_EVT_MESSAGE_LOCALE_NOT_FOUND"
displaySystemErrorCode' ERROR_EVT_VERSION_TOO_OLD                                                = Just "ERROR_EVT_VERSION_TOO_OLD"
displaySystemErrorCode' ERROR_EVT_VERSION_TOO_NEW                                                = Just "ERROR_EVT_VERSION_TOO_NEW"
displaySystemErrorCode' ERROR_EVT_CANNOT_OPEN_CHANNEL_OF_QUERY                                   = Just "ERROR_EVT_CANNOT_OPEN_CHANNEL_OF_QUERY"
displaySystemErrorCode' ERROR_EVT_PUBLISHER_DISABLED                                             = Just "ERROR_EVT_PUBLISHER_DISABLED"
displaySystemErrorCode' ERROR_EVT_FILTER_OUT_OF_RANGE                                            = Just "ERROR_EVT_FILTER_OUT_OF_RANGE"
displaySystemErrorCode' ERROR_EC_SUBSCRIPTION_CANNOT_ACTIVATE                                    = Just "ERROR_EC_SUBSCRIPTION_CANNOT_ACTIVATE"
displaySystemErrorCode' ERROR_EC_LOG_DISABLED                                                    = Just "ERROR_EC_LOG_DISABLED"
displaySystemErrorCode' ERROR_EC_CIRCULAR_FORWARDING                                             = Just "ERROR_EC_CIRCULAR_FORWARDING"
displaySystemErrorCode' ERROR_EC_CREDSTORE_FULL                                                  = Just "ERROR_EC_CREDSTORE_FULL"
displaySystemErrorCode' ERROR_EC_CRED_NOT_FOUND                                                  = Just "ERROR_EC_CRED_NOT_FOUND"
displaySystemErrorCode' ERROR_EC_NO_ACTIVE_CHANNEL                                               = Just "ERROR_EC_NO_ACTIVE_CHANNEL"
displaySystemErrorCode' ERROR_MUI_FILE_NOT_FOUND                                                 = Just "ERROR_MUI_FILE_NOT_FOUND"
displaySystemErrorCode' ERROR_MUI_INVALID_FILE                                                   = Just "ERROR_MUI_INVALID_FILE"
displaySystemErrorCode' ERROR_MUI_INVALID_RC_CONFIG                                              = Just "ERROR_MUI_INVALID_RC_CONFIG"
displaySystemErrorCode' ERROR_MUI_INVALID_LOCALE_NAME                                            = Just "ERROR_MUI_INVALID_LOCALE_NAME"
displaySystemErrorCode' ERROR_MUI_INVALID_ULTIMATEFALLBACK_NAME                                  = Just "ERROR_MUI_INVALID_ULTIMATEFALLBACK_NAME"
displaySystemErrorCode' ERROR_MUI_FILE_NOT_LOADED                                                = Just "ERROR_MUI_FILE_NOT_LOADED"
displaySystemErrorCode' ERROR_RESOURCE_ENUM_USER_STOP                                            = Just "ERROR_RESOURCE_ENUM_USER_STOP"
displaySystemErrorCode' ERROR_MUI_INTLSETTINGS_UILANG_NOT_INSTALLED                              = Just "ERROR_MUI_INTLSETTINGS_UILANG_NOT_INSTALLED"
displaySystemErrorCode' ERROR_MUI_INTLSETTINGS_INVALID_LOCALE_NAME                               = Just "ERROR_MUI_INTLSETTINGS_INVALID_LOCALE_NAME"
displaySystemErrorCode' ERROR_MRM_RUNTIME_NO_DEFAULT_OR_NEUTRAL_RESOURCE                         = Just "ERROR_MRM_RUNTIME_NO_DEFAULT_OR_NEUTRAL_RESOURCE"
displaySystemErrorCode' ERROR_MRM_INVALID_PRICONFIG                                              = Just "ERROR_MRM_INVALID_PRICONFIG"
displaySystemErrorCode' ERROR_MRM_INVALID_FILE_TYPE                                              = Just "ERROR_MRM_INVALID_FILE_TYPE"
displaySystemErrorCode' ERROR_MRM_UNKNOWN_QUALIFIER                                              = Just "ERROR_MRM_UNKNOWN_QUALIFIER"
displaySystemErrorCode' ERROR_MRM_INVALID_QUALIFIER_VALUE                                        = Just "ERROR_MRM_INVALID_QUALIFIER_VALUE"
displaySystemErrorCode' ERROR_MRM_NO_CANDIDATE                                                   = Just "ERROR_MRM_NO_CANDIDATE"
displaySystemErrorCode' ERROR_MRM_NO_MATCH_OR_DEFAULT_CANDIDATE                                  = Just "ERROR_MRM_NO_MATCH_OR_DEFAULT_CANDIDATE"
displaySystemErrorCode' ERROR_MRM_RESOURCE_TYPE_MISMATCH                                         = Just "ERROR_MRM_RESOURCE_TYPE_MISMATCH"
displaySystemErrorCode' ERROR_MRM_DUPLICATE_MAP_NAME                                             = Just "ERROR_MRM_DUPLICATE_MAP_NAME"
displaySystemErrorCode' ERROR_MRM_DUPLICATE_ENTRY                                                = Just "ERROR_MRM_DUPLICATE_ENTRY"
displaySystemErrorCode' ERROR_MRM_INVALID_RESOURCE_IDENTIFIER                                    = Just "ERROR_MRM_INVALID_RESOURCE_IDENTIFIER"
displaySystemErrorCode' ERROR_MRM_FILEPATH_TOO_LONG                                              = Just "ERROR_MRM_FILEPATH_TOO_LONG"
displaySystemErrorCode' ERROR_MRM_UNSUPPORTED_DIRECTORY_TYPE                                     = Just "ERROR_MRM_UNSUPPORTED_DIRECTORY_TYPE"
displaySystemErrorCode' ERROR_MRM_INVALID_PRI_FILE                                               = Just "ERROR_MRM_INVALID_PRI_FILE"
displaySystemErrorCode' ERROR_MRM_NAMED_RESOURCE_NOT_FOUND                                       = Just "ERROR_MRM_NAMED_RESOURCE_NOT_FOUND"
displaySystemErrorCode' ERROR_MRM_MAP_NOT_FOUND                                                  = Just "ERROR_MRM_MAP_NOT_FOUND"
displaySystemErrorCode' ERROR_MRM_UNSUPPORTED_PROFILE_TYPE                                       = Just "ERROR_MRM_UNSUPPORTED_PROFILE_TYPE"
displaySystemErrorCode' ERROR_MRM_INVALID_QUALIFIER_OPERATOR                                     = Just "ERROR_MRM_INVALID_QUALIFIER_OPERATOR"
displaySystemErrorCode' ERROR_MRM_INDETERMINATE_QUALIFIER_VALUE                                  = Just "ERROR_MRM_INDETERMINATE_QUALIFIER_VALUE"
displaySystemErrorCode' ERROR_MRM_AUTOMERGE_ENABLED                                              = Just "ERROR_MRM_AUTOMERGE_ENABLED"
displaySystemErrorCode' ERROR_MRM_TOO_MANY_RESOURCES                                             = Just "ERROR_MRM_TOO_MANY_RESOURCES"
displaySystemErrorCode' ERROR_MRM_UNSUPPORTED_FILE_TYPE_FOR_MERGE                                = Just "ERROR_MRM_UNSUPPORTED_FILE_TYPE_FOR_MERGE"
displaySystemErrorCode' ERROR_MRM_UNSUPPORTED_FILE_TYPE_FOR_LOAD_UNLOAD_PRI_FILE                 = Just "ERROR_MRM_UNSUPPORTED_FILE_TYPE_FOR_LOAD_UNLOAD_PRI_FILE"
displaySystemErrorCode' ERROR_MRM_NO_CURRENT_VIEW_ON_THREAD                                      = Just "ERROR_MRM_NO_CURRENT_VIEW_ON_THREAD"
displaySystemErrorCode' ERROR_DIFFERENT_PROFILE_RESOURCE_MANAGER_EXIST                           = Just "ERROR_DIFFERENT_PROFILE_RESOURCE_MANAGER_EXIST"
displaySystemErrorCode' ERROR_OPERATION_NOT_ALLOWED_FROM_SYSTEM_COMPONENT                        = Just "ERROR_OPERATION_NOT_ALLOWED_FROM_SYSTEM_COMPONENT"
displaySystemErrorCode' ERROR_MRM_DIRECT_REF_TO_NON_DEFAULT_RESOURCE                             = Just "ERROR_MRM_DIRECT_REF_TO_NON_DEFAULT_RESOURCE"
displaySystemErrorCode' ERROR_MRM_GENERATION_COUNT_MISMATCH                                      = Just "ERROR_MRM_GENERATION_COUNT_MISMATCH"
displaySystemErrorCode' ERROR_MCA_INVALID_CAPABILITIES_STRING                                    = Just "ERROR_MCA_INVALID_CAPABILITIES_STRING"
displaySystemErrorCode' ERROR_MCA_INVALID_VCP_VERSION                                            = Just "ERROR_MCA_INVALID_VCP_VERSION"
displaySystemErrorCode' ERROR_MCA_MONITOR_VIOLATES_MCCS_SPECIFICATION                            = Just "ERROR_MCA_MONITOR_VIOLATES_MCCS_SPECIFICATION"
displaySystemErrorCode' ERROR_MCA_MCCS_VERSION_MISMATCH                                          = Just "ERROR_MCA_MCCS_VERSION_MISMATCH"
displaySystemErrorCode' ERROR_MCA_UNSUPPORTED_MCCS_VERSION                                       = Just "ERROR_MCA_UNSUPPORTED_MCCS_VERSION"
displaySystemErrorCode' ERROR_MCA_INTERNAL_ERROR                                                 = Just "ERROR_MCA_INTERNAL_ERROR"
displaySystemErrorCode' ERROR_MCA_INVALID_TECHNOLOGY_TYPE_RETURNED                               = Just "ERROR_MCA_INVALID_TECHNOLOGY_TYPE_RETURNED"
displaySystemErrorCode' ERROR_MCA_UNSUPPORTED_COLOR_TEMPERATURE                                  = Just "ERROR_MCA_UNSUPPORTED_COLOR_TEMPERATURE"
displaySystemErrorCode' ERROR_AMBIGUOUS_SYSTEM_DEVICE                                            = Just "ERROR_AMBIGUOUS_SYSTEM_DEVICE"
displaySystemErrorCode' ERROR_SYSTEM_DEVICE_NOT_FOUND                                            = Just "ERROR_SYSTEM_DEVICE_NOT_FOUND"
displaySystemErrorCode' ERROR_HASH_NOT_SUPPORTED                                                 = Just "ERROR_HASH_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_HASH_NOT_PRESENT                                                   = Just "ERROR_HASH_NOT_PRESENT"
displaySystemErrorCode' ERROR_SECONDARY_IC_PROVIDER_NOT_REGISTERED                               = Just "ERROR_SECONDARY_IC_PROVIDER_NOT_REGISTERED"
displaySystemErrorCode' ERROR_GPIO_CLIENT_INFORMATION_INVALID                                    = Just "ERROR_GPIO_CLIENT_INFORMATION_INVALID"
displaySystemErrorCode' ERROR_GPIO_VERSION_NOT_SUPPORTED                                         = Just "ERROR_GPIO_VERSION_NOT_SUPPORTED"
displaySystemErrorCode' ERROR_GPIO_INVALID_REGISTRATION_PACKET                                   = Just "ERROR_GPIO_INVALID_REGISTRATION_PACKET"
displaySystemErrorCode' ERROR_GPIO_OPERATION_DENIED                                              = Just "ERROR_GPIO_OPERATION_DENIED"
displaySystemErrorCode' ERROR_GPIO_INCOMPATIBLE_CONNECT_MODE                                     = Just "ERROR_GPIO_INCOMPATIBLE_CONNECT_MODE"
displaySystemErrorCode' ERROR_GPIO_INTERRUPT_ALREADY_UNMASKED                                    = Just "ERROR_GPIO_INTERRUPT_ALREADY_UNMASKED"
displaySystemErrorCode' ERROR_CANNOT_SWITCH_RUNLEVEL                                             = Just "ERROR_CANNOT_SWITCH_RUNLEVEL"
displaySystemErrorCode' ERROR_INVALID_RUNLEVEL_SETTING                                           = Just "ERROR_INVALID_RUNLEVEL_SETTING"
displaySystemErrorCode' ERROR_RUNLEVEL_SWITCH_TIMEOUT                                            = Just "ERROR_RUNLEVEL_SWITCH_TIMEOUT"
displaySystemErrorCode' ERROR_RUNLEVEL_SWITCH_AGENT_TIMEOUT                                      = Just "ERROR_RUNLEVEL_SWITCH_AGENT_TIMEOUT"
displaySystemErrorCode' ERROR_RUNLEVEL_SWITCH_IN_PROGRESS                                        = Just "ERROR_RUNLEVEL_SWITCH_IN_PROGRESS"
displaySystemErrorCode' ERROR_SERVICES_FAILED_AUTOSTART                                          = Just "ERROR_SERVICES_FAILED_AUTOSTART"
displaySystemErrorCode' ERROR_COM_TASK_STOP_PENDING                                              = Just "ERROR_COM_TASK_STOP_PENDING"
displaySystemErrorCode' ERROR_INSTALL_OPEN_PACKAGE_FAILED                                        = Just "ERROR_INSTALL_OPEN_PACKAGE_FAILED"
displaySystemErrorCode' ERROR_INSTALL_PACKAGE_NOT_FOUND                                          = Just "ERROR_INSTALL_PACKAGE_NOT_FOUND"
displaySystemErrorCode' ERROR_INSTALL_INVALID_PACKAGE                                            = Just "ERROR_INSTALL_INVALID_PACKAGE"
displaySystemErrorCode' ERROR_INSTALL_RESOLVE_DEPENDENCY_FAILED                                  = Just "ERROR_INSTALL_RESOLVE_DEPENDENCY_FAILED"
displaySystemErrorCode' ERROR_INSTALL_OUT_OF_DISK_SPACE                                          = Just "ERROR_INSTALL_OUT_OF_DISK_SPACE"
displaySystemErrorCode' ERROR_INSTALL_NETWORK_FAILURE                                            = Just "ERROR_INSTALL_NETWORK_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_REGISTRATION_FAILURE                                       = Just "ERROR_INSTALL_REGISTRATION_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_DEREGISTRATION_FAILURE                                     = Just "ERROR_INSTALL_DEREGISTRATION_FAILURE"
displaySystemErrorCode' ERROR_INSTALL_CANCEL                                                     = Just "ERROR_INSTALL_CANCEL"
displaySystemErrorCode' ERROR_INSTALL_FAILED                                                     = Just "ERROR_INSTALL_FAILED"
displaySystemErrorCode' ERROR_REMOVE_FAILED                                                      = Just "ERROR_REMOVE_FAILED"
displaySystemErrorCode' ERROR_PACKAGE_ALREADY_EXISTS                                             = Just "ERROR_PACKAGE_ALREADY_EXISTS"
displaySystemErrorCode' ERROR_NEEDS_REMEDIATION                                                  = Just "ERROR_NEEDS_REMEDIATION"
displaySystemErrorCode' ERROR_INSTALL_PREREQUISITE_FAILED                                        = Just "ERROR_INSTALL_PREREQUISITE_FAILED"
displaySystemErrorCode' ERROR_PACKAGE_REPOSITORY_CORRUPTED                                       = Just "ERROR_PACKAGE_REPOSITORY_CORRUPTED"
displaySystemErrorCode' ERROR_INSTALL_POLICY_FAILURE                                             = Just "ERROR_INSTALL_POLICY_FAILURE"
displaySystemErrorCode' ERROR_PACKAGE_UPDATING                                                   = Just "ERROR_PACKAGE_UPDATING"
displaySystemErrorCode' ERROR_DEPLOYMENT_BLOCKED_BY_POLICY                                       = Just "ERROR_DEPLOYMENT_BLOCKED_BY_POLICY"
displaySystemErrorCode' ERROR_PACKAGES_IN_USE                                                    = Just "ERROR_PACKAGES_IN_USE"
displaySystemErrorCode' ERROR_RECOVERY_FILE_CORRUPT                                              = Just "ERROR_RECOVERY_FILE_CORRUPT"
displaySystemErrorCode' ERROR_INVALID_STAGED_SIGNATURE                                           = Just "ERROR_INVALID_STAGED_SIGNATURE"
displaySystemErrorCode' ERROR_DELETING_EXISTING_APPLICATIONDATA_STORE_FAILED                     = Just "ERROR_DELETING_EXISTING_APPLICATIONDATA_STORE_FAILED"
displaySystemErrorCode' ERROR_INSTALL_PACKAGE_DOWNGRADE                                          = Just "ERROR_INSTALL_PACKAGE_DOWNGRADE"
displaySystemErrorCode' ERROR_SYSTEM_NEEDS_REMEDIATION                                           = Just "ERROR_SYSTEM_NEEDS_REMEDIATION"
displaySystemErrorCode' ERROR_APPX_INTEGRITY_FAILURE_CLR_NGEN                                    = Just "ERROR_APPX_INTEGRITY_FAILURE_CLR_NGEN"
displaySystemErrorCode' ERROR_RESILIENCY_FILE_CORRUPT                                            = Just "ERROR_RESILIENCY_FILE_CORRUPT"
displaySystemErrorCode' ERROR_INSTALL_FIREWALL_SERVICE_NOT_RUNNING                               = Just "ERROR_INSTALL_FIREWALL_SERVICE_NOT_RUNNING"
displaySystemErrorCode' ERROR_STATE_LOAD_STORE_FAILED                                            = Just "ERROR_STATE_LOAD_STORE_FAILED"
displaySystemErrorCode' ERROR_STATE_GET_VERSION_FAILED                                           = Just "ERROR_STATE_GET_VERSION_FAILED"
displaySystemErrorCode' ERROR_STATE_SET_VERSION_FAILED                                           = Just "ERROR_STATE_SET_VERSION_FAILED"
displaySystemErrorCode' ERROR_STATE_STRUCTURED_RESET_FAILED                                      = Just "ERROR_STATE_STRUCTURED_RESET_FAILED"
displaySystemErrorCode' ERROR_STATE_OPEN_CONTAINER_FAILED                                        = Just "ERROR_STATE_OPEN_CONTAINER_FAILED"
displaySystemErrorCode' ERROR_STATE_CREATE_CONTAINER_FAILED                                      = Just "ERROR_STATE_CREATE_CONTAINER_FAILED"
displaySystemErrorCode' ERROR_STATE_DELETE_CONTAINER_FAILED                                      = Just "ERROR_STATE_DELETE_CONTAINER_FAILED"
displaySystemErrorCode' ERROR_STATE_READ_SETTING_FAILED                                          = Just "ERROR_STATE_READ_SETTING_FAILED"
displaySystemErrorCode' ERROR_STATE_WRITE_SETTING_FAILED                                         = Just "ERROR_STATE_WRITE_SETTING_FAILED"
displaySystemErrorCode' ERROR_STATE_DELETE_SETTING_FAILED                                        = Just "ERROR_STATE_DELETE_SETTING_FAILED"
displaySystemErrorCode' ERROR_STATE_QUERY_SETTING_FAILED                                         = Just "ERROR_STATE_QUERY_SETTING_FAILED"
displaySystemErrorCode' ERROR_STATE_READ_COMPOSITE_SETTING_FAILED                                = Just "ERROR_STATE_READ_COMPOSITE_SETTING_FAILED"
displaySystemErrorCode' ERROR_STATE_WRITE_COMPOSITE_SETTING_FAILED                               = Just "ERROR_STATE_WRITE_COMPOSITE_SETTING_FAILED"
displaySystemErrorCode' ERROR_STATE_ENUMERATE_CONTAINER_FAILED                                   = Just "ERROR_STATE_ENUMERATE_CONTAINER_FAILED"
displaySystemErrorCode' ERROR_STATE_ENUMERATE_SETTINGS_FAILED                                    = Just "ERROR_STATE_ENUMERATE_SETTINGS_FAILED"
displaySystemErrorCode' ERROR_STATE_COMPOSITE_SETTING_VALUE_SIZE_LIMIT_EXCEEDED                  = Just "ERROR_STATE_COMPOSITE_SETTING_VALUE_SIZE_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_STATE_SETTING_VALUE_SIZE_LIMIT_EXCEEDED                            = Just "ERROR_STATE_SETTING_VALUE_SIZE_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_STATE_SETTING_NAME_SIZE_LIMIT_EXCEEDED                             = Just "ERROR_STATE_SETTING_NAME_SIZE_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_STATE_CONTAINER_NAME_SIZE_LIMIT_EXCEEDED                           = Just "ERROR_STATE_CONTAINER_NAME_SIZE_LIMIT_EXCEEDED"
displaySystemErrorCode' ERROR_API_UNAVAILABLE                                                    = Just "ERROR_API_UNAVAILABLE"
displaySystemErrorCode' _                                                                        = Nothing

pattern ERROR_SUCCESS                   = 0 :: SystemErrorCode

pattern ERROR_INVALID_FUNCTION          = 1 :: SystemErrorCode

pattern ERROR_FILE_NOT_FOUND            = 2 :: SystemErrorCode

pattern ERROR_PATH_NOT_FOUND            = 3 :: SystemErrorCode

pattern ERROR_TOO_MANY_OPEN_FILES       = 4 :: SystemErrorCode

pattern ERROR_ACCESS_DENIED             = 5 :: SystemErrorCode

pattern ERROR_INVALID_HANDLE            = 6 :: SystemErrorCode

pattern ERROR_ARENA_TRASHED             = 7 :: SystemErrorCode

pattern ERROR_NOT_ENOUGH_MEMORY         = 8 :: SystemErrorCode

pattern ERROR_INVALID_BLOCK             = 9 :: SystemErrorCode

pattern ERROR_BAD_ENVIRONMENT           = 10 :: SystemErrorCode

pattern ERROR_BAD_FORMAT                = 11 :: SystemErrorCode

pattern ERROR_INVALID_ACCESS            = 12 :: SystemErrorCode

pattern ERROR_INVALID_DATA              = 13 :: SystemErrorCode

pattern ERROR_OUTOFMEMORY               = 14 :: SystemErrorCode

pattern ERROR_INVALID_DRIVE             = 15 :: SystemErrorCode

pattern ERROR_CURRENT_DIRECTORY         = 16 :: SystemErrorCode

pattern ERROR_NOT_SAME_DEVICE           = 17 :: SystemErrorCode

pattern ERROR_NO_MORE_FILES             = 18 :: SystemErrorCode

pattern ERROR_WRITE_PROTECT             = 19 :: SystemErrorCode

pattern ERROR_BAD_UNIT                  = 20 :: SystemErrorCode

pattern ERROR_NOT_READY                 = 21 :: SystemErrorCode

pattern ERROR_BAD_COMMAND               = 22 :: SystemErrorCode

pattern ERROR_CRC                       = 23 :: SystemErrorCode

pattern ERROR_BAD_LENGTH                = 24 :: SystemErrorCode

pattern ERROR_SEEK                      = 25 :: SystemErrorCode

pattern ERROR_NOT_DOS_DISK              = 26 :: SystemErrorCode

pattern ERROR_SECTOR_NOT_FOUND          = 27 :: SystemErrorCode

pattern ERROR_OUT_OF_PAPER              = 28 :: SystemErrorCode

pattern ERROR_WRITE_FAULT               = 29 :: SystemErrorCode

pattern ERROR_READ_FAULT                = 30 :: SystemErrorCode

pattern ERROR_GEN_FAILURE               = 31 :: SystemErrorCode

pattern ERROR_SHARING_VIOLATION         = 32 :: SystemErrorCode

pattern ERROR_LOCK_VIOLATION            = 33 :: SystemErrorCode

pattern ERROR_WRONG_DISK                = 34 :: SystemErrorCode

pattern ERROR_SHARING_BUFFER_EXCEEDED   = 36 :: SystemErrorCode

pattern ERROR_HANDLE_EOF                = 38 :: SystemErrorCode

pattern ERROR_HANDLE_DISK_FULL          = 39 :: SystemErrorCode

pattern ERROR_NOT_SUPPORTED             = 50 :: SystemErrorCode

pattern ERROR_REM_NOT_LIST              = 51 :: SystemErrorCode

pattern ERROR_DUP_NAME                  = 52 :: SystemErrorCode

pattern ERROR_BAD_NETPATH               = 53 :: SystemErrorCode

pattern ERROR_NETWORK_BUSY              = 54 :: SystemErrorCode

pattern ERROR_DEV_NOT_EXIST             = 55 :: SystemErrorCode

pattern ERROR_TOO_MANY_CMDS             = 56 :: SystemErrorCode

pattern ERROR_ADAP_HDW_ERR              = 57 :: SystemErrorCode

pattern ERROR_BAD_NET_RESP              = 58 :: SystemErrorCode

pattern ERROR_UNEXP_NET_ERR             = 59 :: SystemErrorCode

pattern ERROR_BAD_REM_ADAP              = 60 :: SystemErrorCode

pattern ERROR_PRINTQ_FULL               = 61 :: SystemErrorCode

pattern ERROR_NO_SPOOL_SPACE            = 62 :: SystemErrorCode

pattern ERROR_PRINT_CANCELLED           = 63 :: SystemErrorCode

pattern ERROR_NETNAME_DELETED           = 64 :: SystemErrorCode

pattern ERROR_NETWORK_ACCESS_DENIED     = 65 :: SystemErrorCode

pattern ERROR_BAD_DEV_TYPE              = 66 :: SystemErrorCode

pattern ERROR_BAD_NET_NAME              = 67 :: SystemErrorCode

pattern ERROR_TOO_MANY_NAMES            = 68 :: SystemErrorCode

pattern ERROR_TOO_MANY_SESS             = 69 :: SystemErrorCode

pattern ERROR_SHARING_PAUSED            = 70 :: SystemErrorCode

pattern ERROR_REQ_NOT_ACCEP             = 71 :: SystemErrorCode

pattern ERROR_REDIR_PAUSED              = 72 :: SystemErrorCode

pattern ERROR_FILE_EXISTS               = 80 :: SystemErrorCode

pattern ERROR_CANNOT_MAKE               = 82 :: SystemErrorCode

pattern ERROR_FAIL_I24                  = 83 :: SystemErrorCode

pattern ERROR_OUT_OF_STRUCTURES         = 84 :: SystemErrorCode

pattern ERROR_ALREADY_ASSIGNED          = 85 :: SystemErrorCode

pattern ERROR_INVALID_PASSWORD          = 86 :: SystemErrorCode

pattern ERROR_INVALID_PARAMETER         = 87 :: SystemErrorCode

pattern ERROR_NET_WRITE_FAULT           = 88 :: SystemErrorCode

pattern ERROR_NO_PROC_SLOTS             = 89 :: SystemErrorCode

pattern ERROR_TOO_MANY_SEMAPHORES       = 100 :: SystemErrorCode

pattern ERROR_EXCL_SEM_ALREADY_OWNED    = 101 :: SystemErrorCode

pattern ERROR_SEM_IS_SET                = 102 :: SystemErrorCode

pattern ERROR_TOO_MANY_SEM_REQUESTS     = 103 :: SystemErrorCode

pattern ERROR_INVALID_AT_INTERRUPT_TIME = 104 :: SystemErrorCode

pattern ERROR_SEM_OWNER_DIED            = 105 :: SystemErrorCode

pattern ERROR_SEM_USER_LIMIT            = 106 :: SystemErrorCode

pattern ERROR_DISK_CHANGE               = 107 :: SystemErrorCode

pattern ERROR_DRIVE_LOCKED              = 108 :: SystemErrorCode

pattern ERROR_BROKEN_PIPE               = 109 :: SystemErrorCode

pattern ERROR_OPEN_FAILED               = 110 :: SystemErrorCode

pattern ERROR_BUFFER_OVERFLOW           = 111 :: SystemErrorCode

pattern ERROR_DISK_FULL                 = 112 :: SystemErrorCode

pattern ERROR_NO_MORE_SEARCH_HANDLES    = 113 :: SystemErrorCode

pattern ERROR_INVALID_TARGET_HANDLE     = 114 :: SystemErrorCode

pattern ERROR_INVALID_CATEGORY          = 117 :: SystemErrorCode

pattern ERROR_INVALID_VERIFY_SWITCH     = 118 :: SystemErrorCode

pattern ERROR_BAD_DRIVER_LEVEL          = 119 :: SystemErrorCode

pattern ERROR_CALL_NOT_IMPLEMENTED      = 120 :: SystemErrorCode

pattern ERROR_SEM_TIMEOUT               = 121 :: SystemErrorCode

pattern ERROR_INSUFFICIENT_BUFFER       = 122 :: SystemErrorCode

pattern ERROR_INVALID_NAME              = 123 :: SystemErrorCode

pattern ERROR_INVALID_LEVEL             = 124 :: SystemErrorCode

pattern ERROR_NO_VOLUME_LABEL           = 125 :: SystemErrorCode

pattern ERROR_MOD_NOT_FOUND             = 126 :: SystemErrorCode

pattern ERROR_PROC_NOT_FOUND            = 127 :: SystemErrorCode

pattern ERROR_WAIT_NO_CHILDREN          = 128 :: SystemErrorCode

pattern ERROR_CHILD_NOT_COMPLETE        = 129 :: SystemErrorCode

pattern ERROR_DIRECT_ACCESS_HANDLE      = 130 :: SystemErrorCode

pattern ERROR_NEGATIVE_SEEK             = 131 :: SystemErrorCode

pattern ERROR_SEEK_ON_DEVICE            = 132 :: SystemErrorCode

pattern ERROR_IS_JOIN_TARGET            = 133 :: SystemErrorCode

pattern ERROR_IS_JOINED                 = 134 :: SystemErrorCode

pattern ERROR_IS_SUBSTED                = 135 :: SystemErrorCode

pattern ERROR_NOT_JOINED                = 136 :: SystemErrorCode

pattern ERROR_NOT_SUBSTED               = 137 :: SystemErrorCode

pattern ERROR_JOIN_TO_JOIN              = 138 :: SystemErrorCode

pattern ERROR_SUBST_TO_SUBST            = 139 :: SystemErrorCode

pattern ERROR_JOIN_TO_SUBST             = 140 :: SystemErrorCode

pattern ERROR_SUBST_TO_JOIN             = 141 :: SystemErrorCode

pattern ERROR_BUSY_DRIVE                = 142 :: SystemErrorCode

pattern ERROR_SAME_DRIVE                = 143 :: SystemErrorCode

pattern ERROR_DIR_NOT_ROOT              = 144 :: SystemErrorCode

pattern ERROR_DIR_NOT_EMPTY             = 145 :: SystemErrorCode

pattern ERROR_IS_SUBST_PATH             = 146 :: SystemErrorCode

pattern ERROR_IS_JOIN_PATH              = 147 :: SystemErrorCode

pattern ERROR_PATH_BUSY                 = 148 :: SystemErrorCode

pattern ERROR_IS_SUBST_TARGET           = 149 :: SystemErrorCode

pattern ERROR_SYSTEM_TRACE              = 150 :: SystemErrorCode

pattern ERROR_INVALID_EVENT_COUNT       = 151 :: SystemErrorCode

pattern ERROR_TOO_MANY_MUXWAITERS       = 152 :: SystemErrorCode

pattern ERROR_INVALID_LIST_FORMAT       = 153 :: SystemErrorCode

pattern ERROR_LABEL_TOO_LONG            = 154 :: SystemErrorCode

pattern ERROR_TOO_MANY_TCBS             = 155 :: SystemErrorCode

pattern ERROR_SIGNAL_REFUSED            = 156 :: SystemErrorCode

pattern ERROR_DISCARDED                 = 157 :: SystemErrorCode

pattern ERROR_NOT_LOCKED                = 158 :: SystemErrorCode

pattern ERROR_BAD_THREADID_ADDR         = 159 :: SystemErrorCode

pattern ERROR_BAD_ARGUMENTS             = 160 :: SystemErrorCode

pattern ERROR_BAD_PATHNAME              = 161 :: SystemErrorCode

pattern ERROR_SIGNAL_PENDING            = 162 :: SystemErrorCode

pattern ERROR_MAX_THRDS_REACHED         = 164 :: SystemErrorCode

pattern ERROR_LOCK_FAILED               = 167 :: SystemErrorCode

pattern ERROR_BUSY                      = 170 :: SystemErrorCode     -- dderror

pattern ERROR_DEVICE_SUPPORT_IN_PROGRESS= 171 :: SystemErrorCode

pattern ERROR_CANCEL_VIOLATION          = 173 :: SystemErrorCode

pattern ERROR_ATOMIC_LOCKS_NOT_SUPPORTED= 174 :: SystemErrorCode

pattern ERROR_INVALID_SEGMENT_NUMBER    = 180 :: SystemErrorCode

pattern ERROR_INVALID_ORDINAL           = 182 :: SystemErrorCode

pattern ERROR_ALREADY_EXISTS            = 183 :: SystemErrorCode

pattern ERROR_INVALID_FLAG_NUMBER       = 186 :: SystemErrorCode

pattern ERROR_SEM_NOT_FOUND             = 187 :: SystemErrorCode

pattern ERROR_INVALID_STARTING_CODESEG  = 188 :: SystemErrorCode

pattern ERROR_INVALID_STACKSEG          = 189 :: SystemErrorCode

pattern ERROR_INVALID_MODULETYPE        = 190 :: SystemErrorCode

pattern ERROR_INVALID_EXE_SIGNATURE     = 191 :: SystemErrorCode

pattern ERROR_EXE_MARKED_INVALID        = 192 :: SystemErrorCode

pattern ERROR_BAD_EXE_FORMAT            = 193 :: SystemErrorCode

pattern ERROR_ITERATED_DATA_EXCEEDS_64k = 194 :: SystemErrorCode

pattern ERROR_INVALID_MINALLOCSIZE      = 195 :: SystemErrorCode

pattern ERROR_DYNLINK_FROM_INVALID_RING = 196 :: SystemErrorCode

pattern ERROR_IOPL_NOT_ENABLED          = 197 :: SystemErrorCode

pattern ERROR_INVALID_SEGDPL            = 198 :: SystemErrorCode

pattern ERROR_AUTODATASEG_EXCEEDS_64k   = 199 :: SystemErrorCode

pattern ERROR_RING2SEG_MUST_BE_MOVABLE  = 200 :: SystemErrorCode

pattern ERROR_RELOC_CHAIN_XEEDS_SEGLIM  = 201 :: SystemErrorCode

pattern ERROR_INFLOOP_IN_RELOC_CHAIN    = 202 :: SystemErrorCode

pattern ERROR_ENVVAR_NOT_FOUND          = 203 :: SystemErrorCode

pattern ERROR_NO_SIGNAL_SENT            = 205 :: SystemErrorCode

pattern ERROR_FILENAME_EXCED_RANGE      = 206 :: SystemErrorCode

pattern ERROR_RING2_STACK_IN_USE        = 207 :: SystemErrorCode

pattern ERROR_META_EXPANSION_TOO_LONG   = 208 :: SystemErrorCode

pattern ERROR_INVALID_SIGNAL_NUMBER     = 209 :: SystemErrorCode

pattern ERROR_THREAD_1_INACTIVE         = 210 :: SystemErrorCode

pattern ERROR_LOCKED                    = 212 :: SystemErrorCode

pattern ERROR_TOO_MANY_MODULES          = 214 :: SystemErrorCode

pattern ERROR_NESTING_NOT_ALLOWED       = 215 :: SystemErrorCode

pattern ERROR_EXE_MACHINE_TYPE_MISMATCH = 216 :: SystemErrorCode

pattern ERROR_EXE_CANNOT_MODIFY_SIGNED_BINARY= 217 :: SystemErrorCode

pattern ERROR_EXE_CANNOT_MODIFY_STRONG_SIGNED_BINARY= 218 :: SystemErrorCode

pattern ERROR_FILE_CHECKED_OUT          = 220 :: SystemErrorCode

pattern ERROR_CHECKOUT_REQUIRED         = 221 :: SystemErrorCode

pattern ERROR_BAD_FILE_TYPE             = 222 :: SystemErrorCode

pattern ERROR_FILE_TOO_LARGE            = 223 :: SystemErrorCode

pattern ERROR_FORMS_AUTH_REQUIRED       = 224 :: SystemErrorCode

pattern ERROR_VIRUS_INFECTED            = 225 :: SystemErrorCode

pattern ERROR_VIRUS_DELETED             = 226 :: SystemErrorCode

pattern ERROR_PIPE_LOCAL                = 229 :: SystemErrorCode

pattern ERROR_BAD_PIPE                  = 230 :: SystemErrorCode

pattern ERROR_PIPE_BUSY                 = 231 :: SystemErrorCode

pattern ERROR_NO_DATA                   = 232 :: SystemErrorCode

pattern ERROR_PIPE_NOT_CONNECTED        = 233 :: SystemErrorCode

pattern ERROR_MORE_DATA                 = 234 :: SystemErrorCode     -- dderror

pattern ERROR_VC_DISCONNECTED           = 240 :: SystemErrorCode

pattern ERROR_INVALID_EA_NAME           = 254 :: SystemErrorCode

pattern ERROR_EA_LIST_INCONSISTENT      = 255 :: SystemErrorCode

pattern ERROR_NO_MORE_ITEMS             = 259 :: SystemErrorCode

pattern ERROR_CANNOT_COPY               = 266 :: SystemErrorCode

pattern ERROR_DIRECTORY                 = 267 :: SystemErrorCode

pattern ERROR_EAS_DIDNT_FIT             = 275 :: SystemErrorCode

pattern ERROR_EA_FILE_CORRUPT           = 276 :: SystemErrorCode

pattern ERROR_EA_TABLE_FULL             = 277 :: SystemErrorCode

pattern ERROR_INVALID_EA_HANDLE         = 278 :: SystemErrorCode

pattern ERROR_EAS_NOT_SUPPORTED         = 282 :: SystemErrorCode

pattern ERROR_NOT_OWNER                 = 288 :: SystemErrorCode

pattern ERROR_TOO_MANY_POSTS            = 298 :: SystemErrorCode

pattern ERROR_PARTIAL_COPY              = 299 :: SystemErrorCode

pattern ERROR_OPLOCK_NOT_GRANTED        = 300 :: SystemErrorCode

pattern ERROR_INVALID_OPLOCK_PROTOCOL   = 301 :: SystemErrorCode

pattern ERROR_DISK_TOO_FRAGMENTED       = 302 :: SystemErrorCode

pattern ERROR_DELETE_PENDING            = 303 :: SystemErrorCode

pattern ERROR_INCOMPATIBLE_WITH_GLOBAL_SHORT_NAME_REGISTRY_SETTING= 304 :: SystemErrorCode

pattern ERROR_SHORT_NAMES_NOT_ENABLED_ON_VOLUME= 305 :: SystemErrorCode

pattern ERROR_SECURITY_STREAM_IS_INCONSISTENT= 306 :: SystemErrorCode

pattern ERROR_INVALID_LOCK_RANGE        = 307 :: SystemErrorCode

pattern ERROR_IMAGE_SUBSYSTEM_NOT_PRESENT= 308 :: SystemErrorCode

pattern ERROR_NOTIFICATION_GUID_ALREADY_DEFINED= 309 :: SystemErrorCode

pattern ERROR_INVALID_EXCEPTION_HANDLER = 310 :: SystemErrorCode

pattern ERROR_DUPLICATE_PRIVILEGES      = 311 :: SystemErrorCode

pattern ERROR_NO_RANGES_PROCESSED       = 312 :: SystemErrorCode

pattern ERROR_NOT_ALLOWED_ON_SYSTEM_FILE= 313 :: SystemErrorCode

pattern ERROR_DISK_RESOURCES_EXHAUSTED  = 314 :: SystemErrorCode

pattern ERROR_INVALID_TOKEN             = 315 :: SystemErrorCode

pattern ERROR_DEVICE_FEATURE_NOT_SUPPORTED= 316 :: SystemErrorCode

pattern ERROR_MR_MID_NOT_FOUND          = 317 :: SystemErrorCode

pattern ERROR_SCOPE_NOT_FOUND           = 318 :: SystemErrorCode

pattern ERROR_UNDEFINED_SCOPE           = 319 :: SystemErrorCode

pattern ERROR_INVALID_CAP               = 320 :: SystemErrorCode

pattern ERROR_DEVICE_UNREACHABLE        = 321 :: SystemErrorCode

pattern ERROR_DEVICE_NO_RESOURCES       = 322 :: SystemErrorCode

pattern ERROR_DATA_CHECKSUM_ERROR       = 323 :: SystemErrorCode

pattern ERROR_INTERMIXED_KERNEL_EA_OPERATION= 324 :: SystemErrorCode

pattern ERROR_FILE_LEVEL_TRIM_NOT_SUPPORTED= 326 :: SystemErrorCode

pattern ERROR_OFFSET_ALIGNMENT_VIOLATION= 327 :: SystemErrorCode

pattern ERROR_INVALID_FIELD_IN_PARAMETER_LIST= 328 :: SystemErrorCode

pattern ERROR_OPERATION_IN_PROGRESS     = 329 :: SystemErrorCode

pattern ERROR_BAD_DEVICE_PATH           = 330 :: SystemErrorCode

pattern ERROR_TOO_MANY_DESCRIPTORS      = 331 :: SystemErrorCode

pattern ERROR_SCRUB_DATA_DISABLED       = 332 :: SystemErrorCode

pattern ERROR_NOT_REDUNDANT_STORAGE     = 333 :: SystemErrorCode

pattern ERROR_RESIDENT_FILE_NOT_SUPPORTED= 334 :: SystemErrorCode

pattern ERROR_COMPRESSED_FILE_NOT_SUPPORTED= 335 :: SystemErrorCode

pattern ERROR_DIRECTORY_NOT_SUPPORTED   = 336 :: SystemErrorCode

pattern ERROR_NOT_READ_FROM_COPY        = 337 :: SystemErrorCode

pattern ERROR_FT_WRITE_FAILURE          = 338 :: SystemErrorCode

pattern ERROR_FT_DI_SCAN_REQUIRED       = 339 :: SystemErrorCode

pattern ERROR_INVALID_KERNEL_INFO_VERSION= 340 :: SystemErrorCode

pattern ERROR_INVALID_PEP_INFO_VERSION  = 341 :: SystemErrorCode

pattern ERROR_OBJECT_NOT_EXTERNALLY_BACKED= 342 :: SystemErrorCode

pattern ERROR_EXTERNAL_BACKING_PROVIDER_UNKNOWN= 343 :: SystemErrorCode

pattern ERROR_FAIL_NOACTION_REBOOT      = 350 :: SystemErrorCode

pattern ERROR_FAIL_SHUTDOWN             = 351 :: SystemErrorCode

pattern ERROR_FAIL_RESTART              = 352 :: SystemErrorCode

pattern ERROR_MAX_SESSIONS_REACHED      = 353 :: SystemErrorCode

pattern ERROR_THREAD_MODE_ALREADY_BACKGROUND= 400 :: SystemErrorCode

pattern ERROR_THREAD_MODE_NOT_BACKGROUND= 401 :: SystemErrorCode

pattern ERROR_PROCESS_MODE_ALREADY_BACKGROUND= 402 :: SystemErrorCode

pattern ERROR_PROCESS_MODE_NOT_BACKGROUND= 403 :: SystemErrorCode

pattern ERROR_DEVICE_HARDWARE_ERROR     = 483 :: SystemErrorCode

pattern ERROR_INVALID_ADDRESS           = 487 :: SystemErrorCode

pattern ERROR_USER_PROFILE_LOAD         = 500 :: SystemErrorCode

pattern ERROR_ARITHMETIC_OVERFLOW       = 534 :: SystemErrorCode

pattern ERROR_PIPE_CONNECTED            = 535 :: SystemErrorCode

pattern ERROR_PIPE_LISTENING            = 536 :: SystemErrorCode

pattern ERROR_VERIFIER_STOP             = 537 :: SystemErrorCode

pattern ERROR_ABIOS_ERROR               = 538 :: SystemErrorCode

pattern ERROR_WX86_WARNING              = 539 :: SystemErrorCode

pattern ERROR_WX86_ERROR                = 540 :: SystemErrorCode

pattern ERROR_TIMER_NOT_CANCELED        = 541 :: SystemErrorCode

pattern ERROR_UNWIND                    = 542 :: SystemErrorCode

pattern ERROR_BAD_STACK                 = 543 :: SystemErrorCode

pattern ERROR_INVALID_UNWIND_TARGET     = 544 :: SystemErrorCode

pattern ERROR_INVALID_PORT_ATTRIBUTES   = 545 :: SystemErrorCode

pattern ERROR_PORT_MESSAGE_TOO_LONG     = 546 :: SystemErrorCode

pattern ERROR_INVALID_QUOTA_LOWER       = 547 :: SystemErrorCode

pattern ERROR_DEVICE_ALREADY_ATTACHED   = 548 :: SystemErrorCode

pattern ERROR_INSTRUCTION_MISALIGNMENT  = 549 :: SystemErrorCode

pattern ERROR_PROFILING_NOT_STARTED     = 550 :: SystemErrorCode

pattern ERROR_PROFILING_NOT_STOPPED     = 551 :: SystemErrorCode

pattern ERROR_COULD_NOT_INTERPRET       = 552 :: SystemErrorCode

pattern ERROR_PROFILING_AT_LIMIT        = 553 :: SystemErrorCode

pattern ERROR_CANT_WAIT                 = 554 :: SystemErrorCode

pattern ERROR_CANT_TERMINATE_SELF       = 555 :: SystemErrorCode

pattern ERROR_UNEXPECTED_MM_CREATE_ERR  = 556 :: SystemErrorCode

pattern ERROR_UNEXPECTED_MM_MAP_ERROR   = 557 :: SystemErrorCode

pattern ERROR_UNEXPECTED_MM_EXTEND_ERR  = 558 :: SystemErrorCode

pattern ERROR_BAD_FUNCTION_TABLE        = 559 :: SystemErrorCode

pattern ERROR_NO_GUID_TRANSLATION       = 560 :: SystemErrorCode

pattern ERROR_INVALID_LDT_SIZE          = 561 :: SystemErrorCode

pattern ERROR_INVALID_LDT_OFFSET        = 563 :: SystemErrorCode

pattern ERROR_INVALID_LDT_DESCRIPTOR    = 564 :: SystemErrorCode

pattern ERROR_TOO_MANY_THREADS          = 565 :: SystemErrorCode

pattern ERROR_THREAD_NOT_IN_PROCESS     = 566 :: SystemErrorCode

pattern ERROR_PAGEFILE_QUOTA_EXCEEDED   = 567 :: SystemErrorCode

pattern ERROR_LOGON_SERVER_CONFLICT     = 568 :: SystemErrorCode

pattern ERROR_SYNCHRONIZATION_REQUIRED  = 569 :: SystemErrorCode

pattern ERROR_NET_OPEN_FAILED           = 570 :: SystemErrorCode

pattern ERROR_IO_PRIVILEGE_FAILED       = 571 :: SystemErrorCode

pattern ERROR_CONTROL_C_EXIT            = 572 :: SystemErrorCode     -- winnt

pattern ERROR_MISSING_SYSTEMFILE        = 573 :: SystemErrorCode

pattern ERROR_UNHANDLED_EXCEPTION       = 574 :: SystemErrorCode

pattern ERROR_APP_INIT_FAILURE          = 575 :: SystemErrorCode

pattern ERROR_PAGEFILE_CREATE_FAILED    = 576 :: SystemErrorCode

pattern ERROR_INVALID_IMAGE_HASH        = 577 :: SystemErrorCode

pattern ERROR_NO_PAGEFILE               = 578 :: SystemErrorCode

pattern ERROR_ILLEGAL_FLOAT_CONTEXT     = 579 :: SystemErrorCode

pattern ERROR_NO_EVENT_PAIR             = 580 :: SystemErrorCode

pattern ERROR_DOMAIN_CTRLR_CONFIG_ERROR = 581 :: SystemErrorCode

pattern ERROR_ILLEGAL_CHARACTER         = 582 :: SystemErrorCode

pattern ERROR_UNDEFINED_CHARACTER       = 583 :: SystemErrorCode

pattern ERROR_FLOPPY_VOLUME             = 584 :: SystemErrorCode

pattern ERROR_BIOS_FAILED_TO_CONNECT_INTERRUPT= 585 :: SystemErrorCode

pattern ERROR_BACKUP_CONTROLLER         = 586 :: SystemErrorCode

pattern ERROR_MUTANT_LIMIT_EXCEEDED     = 587 :: SystemErrorCode

pattern ERROR_FS_DRIVER_REQUIRED        = 588 :: SystemErrorCode

pattern ERROR_CANNOT_LOAD_REGISTRY_FILE = 589 :: SystemErrorCode

pattern ERROR_DEBUG_ATTACH_FAILED       = 590 :: SystemErrorCode

pattern ERROR_SYSTEM_PROCESS_TERMINATED = 591 :: SystemErrorCode

pattern ERROR_DATA_NOT_ACCEPTED         = 592 :: SystemErrorCode

pattern ERROR_VDM_HARD_ERROR            = 593 :: SystemErrorCode

pattern ERROR_DRIVER_CANCEL_TIMEOUT     = 594 :: SystemErrorCode

pattern ERROR_REPLY_MESSAGE_MISMATCH    = 595 :: SystemErrorCode

pattern ERROR_LOST_WRITEBEHIND_DATA     = 596 :: SystemErrorCode

pattern ERROR_CLIENT_SERVER_PARAMETERS_INVALID= 597 :: SystemErrorCode

pattern ERROR_NOT_TINY_STREAM           = 598 :: SystemErrorCode

pattern ERROR_STACK_OVERFLOW_READ       = 599 :: SystemErrorCode

pattern ERROR_CONVERT_TO_LARGE          = 600 :: SystemErrorCode

pattern ERROR_FOUND_OUT_OF_SCOPE        = 601 :: SystemErrorCode

pattern ERROR_ALLOCATE_BUCKET           = 602 :: SystemErrorCode

pattern ERROR_MARSHALL_OVERFLOW         = 603 :: SystemErrorCode

pattern ERROR_INVALID_VARIANT           = 604 :: SystemErrorCode

pattern ERROR_BAD_COMPRESSION_BUFFER    = 605 :: SystemErrorCode

pattern ERROR_AUDIT_FAILED              = 606 :: SystemErrorCode

pattern ERROR_TIMER_RESOLUTION_NOT_SET  = 607 :: SystemErrorCode

pattern ERROR_INSUFFICIENT_LOGON_INFO   = 608 :: SystemErrorCode

pattern ERROR_BAD_DLL_ENTRYPOINT        = 609 :: SystemErrorCode

pattern ERROR_BAD_SERVICE_ENTRYPOINT    = 610 :: SystemErrorCode

pattern ERROR_IP_ADDRESS_CONFLICT1      = 611 :: SystemErrorCode

pattern ERROR_IP_ADDRESS_CONFLICT2      = 612 :: SystemErrorCode

pattern ERROR_REGISTRY_QUOTA_LIMIT      = 613 :: SystemErrorCode

pattern ERROR_NO_CALLBACK_ACTIVE        = 614 :: SystemErrorCode

pattern ERROR_PWD_TOO_SHORT             = 615 :: SystemErrorCode

pattern ERROR_PWD_TOO_RECENT            = 616 :: SystemErrorCode

pattern ERROR_PWD_HISTORY_CONFLICT      = 617 :: SystemErrorCode

pattern ERROR_UNSUPPORTED_COMPRESSION   = 618 :: SystemErrorCode

pattern ERROR_INVALID_HW_PROFILE        = 619 :: SystemErrorCode

pattern ERROR_INVALID_PLUGPLAY_DEVICE_PATH= 620 :: SystemErrorCode

pattern ERROR_QUOTA_LIST_INCONSISTENT   = 621 :: SystemErrorCode

pattern ERROR_EVALUATION_EXPIRATION     = 622 :: SystemErrorCode

pattern ERROR_ILLEGAL_DLL_RELOCATION    = 623 :: SystemErrorCode

pattern ERROR_DLL_INIT_FAILED_LOGOFF    = 624 :: SystemErrorCode

pattern ERROR_VALIDATE_CONTINUE         = 625 :: SystemErrorCode

pattern ERROR_NO_MORE_MATCHES           = 626 :: SystemErrorCode

pattern ERROR_RANGE_LIST_CONFLICT       = 627 :: SystemErrorCode

pattern ERROR_SERVER_SID_MISMATCH       = 628 :: SystemErrorCode

pattern ERROR_CANT_ENABLE_DENY_ONLY     = 629 :: SystemErrorCode

pattern ERROR_FLOAT_MULTIPLE_FAULTS     = 630 :: SystemErrorCode     -- winnt

pattern ERROR_FLOAT_MULTIPLE_TRAPS      = 631 :: SystemErrorCode     -- winnt

pattern ERROR_NOINTERFACE               = 632 :: SystemErrorCode

pattern ERROR_DRIVER_FAILED_SLEEP       = 633 :: SystemErrorCode

pattern ERROR_CORRUPT_SYSTEM_FILE       = 634 :: SystemErrorCode

pattern ERROR_COMMITMENT_MINIMUM        = 635 :: SystemErrorCode

pattern ERROR_PNP_RESTART_ENUMERATION   = 636 :: SystemErrorCode

pattern ERROR_SYSTEM_IMAGE_BAD_SIGNATURE= 637 :: SystemErrorCode

pattern ERROR_PNP_REBOOT_REQUIRED       = 638 :: SystemErrorCode

pattern ERROR_INSUFFICIENT_POWER        = 639 :: SystemErrorCode

pattern ERROR_MULTIPLE_FAULT_VIOLATION  = 640 :: SystemErrorCode

pattern ERROR_SYSTEM_SHUTDOWN           = 641 :: SystemErrorCode

pattern ERROR_PORT_NOT_SET              = 642 :: SystemErrorCode

pattern ERROR_DS_VERSION_CHECK_FAILURE  = 643 :: SystemErrorCode

pattern ERROR_RANGE_NOT_FOUND           = 644 :: SystemErrorCode

pattern ERROR_NOT_SAFE_MODE_DRIVER      = 646 :: SystemErrorCode

pattern ERROR_FAILED_DRIVER_ENTRY       = 647 :: SystemErrorCode

pattern ERROR_DEVICE_ENUMERATION_ERROR  = 648 :: SystemErrorCode

pattern ERROR_MOUNT_POINT_NOT_RESOLVED  = 649 :: SystemErrorCode

pattern ERROR_INVALID_DEVICE_OBJECT_PARAMETER= 650 :: SystemErrorCode

pattern ERROR_MCA_OCCURED               = 651 :: SystemErrorCode

pattern ERROR_DRIVER_DATABASE_ERROR     = 652 :: SystemErrorCode

pattern ERROR_SYSTEM_HIVE_TOO_LARGE     = 653 :: SystemErrorCode

pattern ERROR_DRIVER_FAILED_PRIOR_UNLOAD= 654 :: SystemErrorCode

pattern ERROR_VOLSNAP_PREPARE_HIBERNATE = 655 :: SystemErrorCode

pattern ERROR_HIBERNATION_FAILURE       = 656 :: SystemErrorCode

pattern ERROR_PWD_TOO_LONG              = 657 :: SystemErrorCode

pattern ERROR_FILE_SYSTEM_LIMITATION    = 665 :: SystemErrorCode

pattern ERROR_ASSERTION_FAILURE         = 668 :: SystemErrorCode

pattern ERROR_ACPI_ERROR                = 669 :: SystemErrorCode

pattern ERROR_WOW_ASSERTION             = 670 :: SystemErrorCode

pattern ERROR_PNP_BAD_MPS_TABLE         = 671 :: SystemErrorCode

pattern ERROR_PNP_TRANSLATION_FAILED    = 672 :: SystemErrorCode

pattern ERROR_PNP_IRQ_TRANSLATION_FAILED= 673 :: SystemErrorCode

pattern ERROR_PNP_INVALID_ID            = 674 :: SystemErrorCode

pattern ERROR_WAKE_SYSTEM_DEBUGGER      = 675 :: SystemErrorCode

pattern ERROR_HANDLES_CLOSED            = 676 :: SystemErrorCode

pattern ERROR_EXTRANEOUS_INFORMATION    = 677 :: SystemErrorCode

pattern ERROR_RXACT_COMMIT_NECESSARY    = 678 :: SystemErrorCode

pattern ERROR_MEDIA_CHECK               = 679 :: SystemErrorCode

pattern ERROR_GUID_SUBSTITUTION_MADE    = 680 :: SystemErrorCode

pattern ERROR_STOPPED_ON_SYMLINK        = 681 :: SystemErrorCode

pattern ERROR_LONGJUMP                  = 682 :: SystemErrorCode

pattern ERROR_PLUGPLAY_QUERY_VETOED     = 683 :: SystemErrorCode

pattern ERROR_UNWIND_CONSOLIDATE        = 684 :: SystemErrorCode

pattern ERROR_REGISTRY_HIVE_RECOVERED   = 685 :: SystemErrorCode

pattern ERROR_DLL_MIGHT_BE_INSECURE     = 686 :: SystemErrorCode

pattern ERROR_DLL_MIGHT_BE_INCOMPATIBLE = 687 :: SystemErrorCode

pattern ERROR_DBG_EXCEPTION_NOT_HANDLED = 688 :: SystemErrorCode     -- winnt

pattern ERROR_DBG_REPLY_LATER           = 689 :: SystemErrorCode

pattern ERROR_DBG_UNABLE_TO_PROVIDE_HANDLE= 690 :: SystemErrorCode

pattern ERROR_DBG_TERMINATE_THREAD      = 691 :: SystemErrorCode     -- winnt

pattern ERROR_DBG_TERMINATE_PROCESS     = 692 :: SystemErrorCode     -- winnt

pattern ERROR_DBG_CONTROL_C             = 693 :: SystemErrorCode     -- winnt

pattern ERROR_DBG_PRINTEXCEPTION_C      = 694 :: SystemErrorCode

pattern ERROR_DBG_RIPEXCEPTION          = 695 :: SystemErrorCode

pattern ERROR_DBG_CONTROL_BREAK         = 696 :: SystemErrorCode     -- winnt

pattern ERROR_DBG_COMMAND_EXCEPTION     = 697 :: SystemErrorCode     -- winnt

pattern ERROR_OBJECT_NAME_EXISTS        = 698 :: SystemErrorCode

pattern ERROR_THREAD_WAS_SUSPENDED      = 699 :: SystemErrorCode

pattern ERROR_IMAGE_NOT_AT_BASE         = 700 :: SystemErrorCode

pattern ERROR_RXACT_STATE_CREATED       = 701 :: SystemErrorCode

pattern ERROR_SEGMENT_NOTIFICATION      = 702 :: SystemErrorCode     -- winnt

pattern ERROR_BAD_CURRENT_DIRECTORY     = 703 :: SystemErrorCode

pattern ERROR_FT_READ_RECOVERY_FROM_BACKUP= 704 :: SystemErrorCode

pattern ERROR_FT_WRITE_RECOVERY         = 705 :: SystemErrorCode

pattern ERROR_IMAGE_MACHINE_TYPE_MISMATCH= 706 :: SystemErrorCode

pattern ERROR_RECEIVE_PARTIAL           = 707 :: SystemErrorCode

pattern ERROR_RECEIVE_EXPEDITED         = 708 :: SystemErrorCode

pattern ERROR_RECEIVE_PARTIAL_EXPEDITED = 709 :: SystemErrorCode

pattern ERROR_EVENT_DONE                = 710 :: SystemErrorCode

pattern ERROR_EVENT_PENDING             = 711 :: SystemErrorCode

pattern ERROR_CHECKING_FILE_SYSTEM      = 712 :: SystemErrorCode

pattern ERROR_FATAL_APP_EXIT            = 713 :: SystemErrorCode

pattern ERROR_PREDEFINED_HANDLE         = 714 :: SystemErrorCode

pattern ERROR_WAS_UNLOCKED              = 715 :: SystemErrorCode

pattern ERROR_SERVICE_NOTIFICATION      = 716 :: SystemErrorCode

pattern ERROR_WAS_LOCKED                = 717 :: SystemErrorCode

pattern ERROR_LOG_HARD_ERROR            = 718 :: SystemErrorCode

pattern ERROR_ALREADY_WIN32             = 719 :: SystemErrorCode

pattern ERROR_IMAGE_MACHINE_TYPE_MISMATCH_EXE= 720 :: SystemErrorCode

pattern ERROR_NO_YIELD_PERFORMED        = 721 :: SystemErrorCode

pattern ERROR_TIMER_RESUME_IGNORED      = 722 :: SystemErrorCode

pattern ERROR_ARBITRATION_UNHANDLED     = 723 :: SystemErrorCode

pattern ERROR_CARDBUS_NOT_SUPPORTED     = 724 :: SystemErrorCode

pattern ERROR_MP_PROCESSOR_MISMATCH     = 725 :: SystemErrorCode

pattern ERROR_HIBERNATED                = 726 :: SystemErrorCode

pattern ERROR_RESUME_HIBERNATION        = 727 :: SystemErrorCode

pattern ERROR_FIRMWARE_UPDATED          = 728 :: SystemErrorCode

pattern ERROR_DRIVERS_LEAKING_LOCKED_PAGES= 729 :: SystemErrorCode

pattern ERROR_WAKE_SYSTEM               = 730 :: SystemErrorCode

pattern ERROR_WAIT_1                    = 731 :: SystemErrorCode

pattern ERROR_WAIT_2                    = 732 :: SystemErrorCode

pattern ERROR_WAIT_3                    = 733 :: SystemErrorCode

pattern ERROR_WAIT_63                   = 734 :: SystemErrorCode

pattern ERROR_ABANDONED_WAIT_0          = 735 :: SystemErrorCode     -- winnt

pattern ERROR_ABANDONED_WAIT_63         = 736 :: SystemErrorCode

pattern ERROR_USER_APC                  = 737 :: SystemErrorCode     -- winnt

pattern ERROR_KERNEL_APC                = 738 :: SystemErrorCode

pattern ERROR_ALERTED                   = 739 :: SystemErrorCode

pattern ERROR_ELEVATION_REQUIRED        = 740 :: SystemErrorCode

pattern ERROR_REPARSE                   = 741 :: SystemErrorCode

pattern ERROR_OPLOCK_BREAK_IN_PROGRESS  = 742 :: SystemErrorCode

pattern ERROR_VOLUME_MOUNTED            = 743 :: SystemErrorCode

pattern ERROR_RXACT_COMMITTED           = 744 :: SystemErrorCode

pattern ERROR_NOTIFY_CLEANUP            = 745 :: SystemErrorCode

pattern ERROR_PRIMARY_TRANSPORT_CONNECT_FAILED= 746 :: SystemErrorCode

pattern ERROR_PAGE_FAULT_TRANSITION     = 747 :: SystemErrorCode

pattern ERROR_PAGE_FAULT_DEMAND_ZERO    = 748 :: SystemErrorCode

pattern ERROR_PAGE_FAULT_COPY_ON_WRITE  = 749 :: SystemErrorCode

pattern ERROR_PAGE_FAULT_GUARD_PAGE     = 750 :: SystemErrorCode

pattern ERROR_PAGE_FAULT_PAGING_FILE    = 751 :: SystemErrorCode

pattern ERROR_CACHE_PAGE_LOCKED         = 752 :: SystemErrorCode

pattern ERROR_CRASH_DUMP                = 753 :: SystemErrorCode

pattern ERROR_BUFFER_ALL_ZEROS          = 754 :: SystemErrorCode

pattern ERROR_REPARSE_OBJECT            = 755 :: SystemErrorCode

pattern ERROR_RESOURCE_REQUIREMENTS_CHANGED= 756 :: SystemErrorCode

pattern ERROR_TRANSLATION_COMPLETE      = 757 :: SystemErrorCode

pattern ERROR_NOTHING_TO_TERMINATE      = 758 :: SystemErrorCode

pattern ERROR_PROCESS_NOT_IN_JOB        = 759 :: SystemErrorCode

pattern ERROR_PROCESS_IN_JOB            = 760 :: SystemErrorCode

pattern ERROR_VOLSNAP_HIBERNATE_READY   = 761 :: SystemErrorCode

pattern ERROR_FSFILTER_OP_COMPLETED_SUCCESSFULLY= 762 :: SystemErrorCode

pattern ERROR_INTERRUPT_VECTOR_ALREADY_CONNECTED= 763 :: SystemErrorCode

pattern ERROR_INTERRUPT_STILL_CONNECTED = 764 :: SystemErrorCode

pattern ERROR_WAIT_FOR_OPLOCK           = 765 :: SystemErrorCode

pattern ERROR_DBG_EXCEPTION_HANDLED     = 766 :: SystemErrorCode     -- winnt

pattern ERROR_DBG_CONTINUE              = 767 :: SystemErrorCode     -- winnt

pattern ERROR_CALLBACK_POP_STACK        = 768 :: SystemErrorCode

pattern ERROR_COMPRESSION_DISABLED      = 769 :: SystemErrorCode

pattern ERROR_CANTFETCHBACKWARDS        = 770 :: SystemErrorCode

pattern ERROR_CANTSCROLLBACKWARDS       = 771 :: SystemErrorCode

pattern ERROR_ROWSNOTRELEASED           = 772 :: SystemErrorCode

pattern ERROR_BAD_ACCESSOR_FLAGS        = 773 :: SystemErrorCode

pattern ERROR_ERRORS_ENCOUNTERED        = 774 :: SystemErrorCode

pattern ERROR_NOT_CAPABLE               = 775 :: SystemErrorCode

pattern ERROR_REQUEST_OUT_OF_SEQUENCE   = 776 :: SystemErrorCode

pattern ERROR_VERSION_PARSE_ERROR       = 777 :: SystemErrorCode

pattern ERROR_BADSTARTPOSITION          = 778 :: SystemErrorCode

pattern ERROR_MEMORY_HARDWARE           = 779 :: SystemErrorCode

pattern ERROR_DISK_REPAIR_DISABLED      = 780 :: SystemErrorCode

pattern ERROR_INSUFFICIENT_RESOURCE_FOR_SPECIFIED_SHARED_SECTION_SIZE= 781 :: SystemErrorCode

pattern ERROR_SYSTEM_POWERSTATE_TRANSITION= 782 :: SystemErrorCode

pattern ERROR_SYSTEM_POWERSTATE_COMPLEX_TRANSITION= 783 :: SystemErrorCode

pattern ERROR_MCA_EXCEPTION             = 784 :: SystemErrorCode

pattern ERROR_ACCESS_AUDIT_BY_POLICY    = 785 :: SystemErrorCode

pattern ERROR_ACCESS_DISABLED_NO_SAFER_UI_BY_POLICY= 786 :: SystemErrorCode

pattern ERROR_ABANDON_HIBERFILE         = 787 :: SystemErrorCode

pattern ERROR_LOST_WRITEBEHIND_DATA_NETWORK_DISCONNECTED= 788 :: SystemErrorCode

pattern ERROR_LOST_WRITEBEHIND_DATA_NETWORK_SERVER_ERROR= 789 :: SystemErrorCode

pattern ERROR_LOST_WRITEBEHIND_DATA_LOCAL_DISK_ERROR= 790 :: SystemErrorCode

pattern ERROR_BAD_MCFG_TABLE            = 791 :: SystemErrorCode

pattern ERROR_DISK_REPAIR_REDIRECTED    = 792 :: SystemErrorCode

pattern ERROR_DISK_REPAIR_UNSUCCESSFUL  = 793 :: SystemErrorCode

pattern ERROR_CORRUPT_LOG_OVERFULL      = 794 :: SystemErrorCode

pattern ERROR_CORRUPT_LOG_CORRUPTED     = 795 :: SystemErrorCode

pattern ERROR_CORRUPT_LOG_UNAVAILABLE   = 796 :: SystemErrorCode

pattern ERROR_CORRUPT_LOG_DELETED_FULL  = 797 :: SystemErrorCode

pattern ERROR_CORRUPT_LOG_CLEARED       = 798 :: SystemErrorCode

pattern ERROR_ORPHAN_NAME_EXHAUSTED     = 799 :: SystemErrorCode

pattern ERROR_OPLOCK_SWITCHED_TO_NEW_HANDLE= 800 :: SystemErrorCode

pattern ERROR_CANNOT_GRANT_REQUESTED_OPLOCK= 801 :: SystemErrorCode

pattern ERROR_CANNOT_BREAK_OPLOCK       = 802 :: SystemErrorCode

pattern ERROR_OPLOCK_HANDLE_CLOSED      = 803 :: SystemErrorCode

pattern ERROR_NO_ACE_CONDITION          = 804 :: SystemErrorCode

pattern ERROR_INVALID_ACE_CONDITION     = 805 :: SystemErrorCode

pattern ERROR_FILE_HANDLE_REVOKED       = 806 :: SystemErrorCode

pattern ERROR_IMAGE_AT_DIFFERENT_BASE   = 807 :: SystemErrorCode

pattern ERROR_ENCRYPTED_IO_NOT_POSSIBLE = 808 :: SystemErrorCode

pattern ERROR_EA_ACCESS_DENIED          = 994 :: SystemErrorCode

pattern ERROR_OPERATION_ABORTED         = 995 :: SystemErrorCode

pattern ERROR_IO_INCOMPLETE             = 996 :: SystemErrorCode

pattern ERROR_IO_PENDING                = 997 :: SystemErrorCode     -- dderror

pattern ERROR_NOACCESS                  = 998 :: SystemErrorCode

pattern ERROR_SWAPERROR                 = 999 :: SystemErrorCode

pattern ERROR_STACK_OVERFLOW            = 1001 :: SystemErrorCode

pattern ERROR_INVALID_MESSAGE           = 1002 :: SystemErrorCode

pattern ERROR_CAN_NOT_COMPLETE          = 1003 :: SystemErrorCode

pattern ERROR_INVALID_FLAGS             = 1004 :: SystemErrorCode

pattern ERROR_UNRECOGNIZED_VOLUME       = 1005 :: SystemErrorCode

pattern ERROR_FILE_INVALID              = 1006 :: SystemErrorCode

pattern ERROR_FULLSCREEN_MODE           = 1007 :: SystemErrorCode

pattern ERROR_NO_TOKEN                  = 1008 :: SystemErrorCode

pattern ERROR_BADDB                     = 1009 :: SystemErrorCode

pattern ERROR_BADKEY                    = 1010 :: SystemErrorCode

pattern ERROR_CANTOPEN                  = 1011 :: SystemErrorCode

pattern ERROR_CANTREAD                  = 1012 :: SystemErrorCode

pattern ERROR_CANTWRITE                 = 1013 :: SystemErrorCode

pattern ERROR_REGISTRY_RECOVERED        = 1014 :: SystemErrorCode

pattern ERROR_REGISTRY_CORRUPT          = 1015 :: SystemErrorCode

pattern ERROR_REGISTRY_IO_FAILED        = 1016 :: SystemErrorCode

pattern ERROR_NOT_REGISTRY_FILE         = 1017 :: SystemErrorCode

pattern ERROR_KEY_DELETED               = 1018 :: SystemErrorCode

pattern ERROR_NO_LOG_SPACE              = 1019 :: SystemErrorCode

pattern ERROR_KEY_HAS_CHILDREN          = 1020 :: SystemErrorCode

pattern ERROR_CHILD_MUST_BE_VOLATILE    = 1021 :: SystemErrorCode

pattern ERROR_NOTIFY_ENUM_DIR           = 1022 :: SystemErrorCode

pattern ERROR_DEPENDENT_SERVICES_RUNNING= 1051 :: SystemErrorCode

pattern ERROR_INVALID_SERVICE_CONTROL   = 1052 :: SystemErrorCode

pattern ERROR_SERVICE_REQUEST_TIMEOUT   = 1053 :: SystemErrorCode

pattern ERROR_SERVICE_NO_THREAD         = 1054 :: SystemErrorCode

pattern ERROR_SERVICE_DATABASE_LOCKED   = 1055 :: SystemErrorCode

pattern ERROR_SERVICE_ALREADY_RUNNING   = 1056 :: SystemErrorCode

pattern ERROR_INVALID_SERVICE_ACCOUNT   = 1057 :: SystemErrorCode

pattern ERROR_SERVICE_DISABLED          = 1058 :: SystemErrorCode

pattern ERROR_CIRCULAR_DEPENDENCY       = 1059 :: SystemErrorCode

pattern ERROR_SERVICE_DOES_NOT_EXIST    = 1060 :: SystemErrorCode

pattern ERROR_SERVICE_CANNOT_ACCEPT_CTRL= 1061 :: SystemErrorCode

pattern ERROR_SERVICE_NOT_ACTIVE        = 1062 :: SystemErrorCode

pattern ERROR_FAILED_SERVICE_CONTROLLER_CONNECT= 1063 :: SystemErrorCode

pattern ERROR_EXCEPTION_IN_SERVICE      = 1064 :: SystemErrorCode

pattern ERROR_DATABASE_DOES_NOT_EXIST   = 1065 :: SystemErrorCode

pattern ERROR_SERVICE_SPECIFIC_ERROR    = 1066 :: SystemErrorCode

pattern ERROR_PROCESS_ABORTED           = 1067 :: SystemErrorCode

pattern ERROR_SERVICE_DEPENDENCY_FAIL   = 1068 :: SystemErrorCode

pattern ERROR_SERVICE_LOGON_FAILED      = 1069 :: SystemErrorCode

pattern ERROR_SERVICE_START_HANG        = 1070 :: SystemErrorCode

pattern ERROR_INVALID_SERVICE_LOCK      = 1071 :: SystemErrorCode

pattern ERROR_SERVICE_MARKED_FOR_DELETE = 1072 :: SystemErrorCode

pattern ERROR_SERVICE_EXISTS            = 1073 :: SystemErrorCode

pattern ERROR_ALREADY_RUNNING_LKG       = 1074 :: SystemErrorCode

pattern ERROR_SERVICE_DEPENDENCY_DELETED= 1075 :: SystemErrorCode

pattern ERROR_BOOT_ALREADY_ACCEPTED     = 1076 :: SystemErrorCode

pattern ERROR_SERVICE_NEVER_STARTED     = 1077 :: SystemErrorCode

pattern ERROR_DUPLICATE_SERVICE_NAME    = 1078 :: SystemErrorCode

pattern ERROR_DIFFERENT_SERVICE_ACCOUNT = 1079 :: SystemErrorCode

pattern ERROR_CANNOT_DETECT_DRIVER_FAILURE= 1080 :: SystemErrorCode

pattern ERROR_CANNOT_DETECT_PROCESS_ABORT= 1081 :: SystemErrorCode

pattern ERROR_NO_RECOVERY_PROGRAM       = 1082 :: SystemErrorCode

pattern ERROR_SERVICE_NOT_IN_EXE        = 1083 :: SystemErrorCode

pattern ERROR_NOT_SAFEBOOT_SERVICE      = 1084 :: SystemErrorCode

pattern ERROR_END_OF_MEDIA              = 1100 :: SystemErrorCode

pattern ERROR_FILEMARK_DETECTED         = 1101 :: SystemErrorCode

pattern ERROR_BEGINNING_OF_MEDIA        = 1102 :: SystemErrorCode

pattern ERROR_SETMARK_DETECTED          = 1103 :: SystemErrorCode

pattern ERROR_NO_DATA_DETECTED          = 1104 :: SystemErrorCode

pattern ERROR_PARTITION_FAILURE         = 1105 :: SystemErrorCode

pattern ERROR_INVALID_BLOCK_LENGTH      = 1106 :: SystemErrorCode

pattern ERROR_DEVICE_NOT_PARTITIONED    = 1107 :: SystemErrorCode

pattern ERROR_UNABLE_TO_LOCK_MEDIA      = 1108 :: SystemErrorCode

pattern ERROR_UNABLE_TO_UNLOAD_MEDIA    = 1109 :: SystemErrorCode

pattern ERROR_MEDIA_CHANGED             = 1110 :: SystemErrorCode

pattern ERROR_BUS_RESET                 = 1111 :: SystemErrorCode

pattern ERROR_NO_MEDIA_IN_DRIVE         = 1112 :: SystemErrorCode

pattern ERROR_NO_UNICODE_TRANSLATION    = 1113 :: SystemErrorCode

pattern ERROR_DLL_INIT_FAILED           = 1114 :: SystemErrorCode

pattern ERROR_SHUTDOWN_IN_PROGRESS      = 1115 :: SystemErrorCode

pattern ERROR_NO_SHUTDOWN_IN_PROGRESS   = 1116 :: SystemErrorCode

pattern ERROR_IO_DEVICE                 = 1117 :: SystemErrorCode

pattern ERROR_SERIAL_NO_DEVICE          = 1118 :: SystemErrorCode

pattern ERROR_IRQ_BUSY                  = 1119 :: SystemErrorCode

pattern ERROR_MORE_WRITES               = 1120 :: SystemErrorCode

pattern ERROR_COUNTER_TIMEOUT           = 1121 :: SystemErrorCode

pattern ERROR_FLOPPY_ID_MARK_NOT_FOUND  = 1122 :: SystemErrorCode

pattern ERROR_FLOPPY_WRONG_CYLINDER     = 1123 :: SystemErrorCode

pattern ERROR_FLOPPY_UNKNOWN_ERROR      = 1124 :: SystemErrorCode

pattern ERROR_FLOPPY_BAD_REGISTERS      = 1125 :: SystemErrorCode

pattern ERROR_DISK_RECALIBRATE_FAILED   = 1126 :: SystemErrorCode

pattern ERROR_DISK_OPERATION_FAILED     = 1127 :: SystemErrorCode

pattern ERROR_DISK_RESET_FAILED         = 1128 :: SystemErrorCode

pattern ERROR_EOM_OVERFLOW              = 1129 :: SystemErrorCode

pattern ERROR_NOT_ENOUGH_SERVER_MEMORY  = 1130 :: SystemErrorCode

pattern ERROR_POSSIBLE_DEADLOCK         = 1131 :: SystemErrorCode

pattern ERROR_MAPPED_ALIGNMENT          = 1132 :: SystemErrorCode

pattern ERROR_SET_POWER_STATE_VETOED    = 1140 :: SystemErrorCode

pattern ERROR_SET_POWER_STATE_FAILED    = 1141 :: SystemErrorCode

pattern ERROR_TOO_MANY_LINKS            = 1142 :: SystemErrorCode

pattern ERROR_OLD_WIN_VERSION           = 1150 :: SystemErrorCode

pattern ERROR_APP_WRONG_OS              = 1151 :: SystemErrorCode

pattern ERROR_SINGLE_INSTANCE_APP       = 1152 :: SystemErrorCode

pattern ERROR_RMODE_APP                 = 1153 :: SystemErrorCode

pattern ERROR_INVALID_DLL               = 1154 :: SystemErrorCode

pattern ERROR_NO_ASSOCIATION            = 1155 :: SystemErrorCode

pattern ERROR_DDE_FAIL                  = 1156 :: SystemErrorCode

pattern ERROR_DLL_NOT_FOUND             = 1157 :: SystemErrorCode

pattern ERROR_NO_MORE_USER_HANDLES      = 1158 :: SystemErrorCode

pattern ERROR_MESSAGE_SYNC_ONLY         = 1159 :: SystemErrorCode

pattern ERROR_SOURCE_ELEMENT_EMPTY      = 1160 :: SystemErrorCode

pattern ERROR_DESTINATION_ELEMENT_FULL  = 1161 :: SystemErrorCode

pattern ERROR_ILLEGAL_ELEMENT_ADDRESS   = 1162 :: SystemErrorCode

pattern ERROR_MAGAZINE_NOT_PRESENT      = 1163 :: SystemErrorCode

pattern ERROR_DEVICE_REINITIALIZATION_NEEDED= 1164 :: SystemErrorCode     -- dderror

pattern ERROR_DEVICE_REQUIRES_CLEANING  = 1165 :: SystemErrorCode

pattern ERROR_DEVICE_DOOR_OPEN          = 1166 :: SystemErrorCode

pattern ERROR_DEVICE_NOT_CONNECTED      = 1167 :: SystemErrorCode

pattern ERROR_NOT_FOUND                 = 1168 :: SystemErrorCode

pattern ERROR_NO_MATCH                  = 1169 :: SystemErrorCode

pattern ERROR_SET_NOT_FOUND             = 1170 :: SystemErrorCode

pattern ERROR_POINT_NOT_FOUND           = 1171 :: SystemErrorCode

pattern ERROR_NO_TRACKING_SERVICE       = 1172 :: SystemErrorCode

pattern ERROR_NO_VOLUME_ID              = 1173 :: SystemErrorCode

pattern ERROR_UNABLE_TO_REMOVE_REPLACED = 1175 :: SystemErrorCode

pattern ERROR_UNABLE_TO_MOVE_REPLACEMENT= 1176 :: SystemErrorCode

pattern ERROR_UNABLE_TO_MOVE_REPLACEMENT_2= 1177 :: SystemErrorCode

pattern ERROR_JOURNAL_DELETE_IN_PROGRESS= 1178 :: SystemErrorCode

pattern ERROR_JOURNAL_NOT_ACTIVE        = 1179 :: SystemErrorCode

pattern ERROR_POTENTIAL_FILE_FOUND      = 1180 :: SystemErrorCode

pattern ERROR_JOURNAL_ENTRY_DELETED     = 1181 :: SystemErrorCode

pattern ERROR_SHUTDOWN_IS_SCHEDULED     = 1190 :: SystemErrorCode

pattern ERROR_SHUTDOWN_USERS_LOGGED_ON  = 1191 :: SystemErrorCode

pattern ERROR_BAD_DEVICE                = 1200 :: SystemErrorCode

pattern ERROR_CONNECTION_UNAVAIL        = 1201 :: SystemErrorCode

pattern ERROR_DEVICE_ALREADY_REMEMBERED = 1202 :: SystemErrorCode

pattern ERROR_NO_NET_OR_BAD_PATH        = 1203 :: SystemErrorCode

pattern ERROR_BAD_PROVIDER              = 1204 :: SystemErrorCode

pattern ERROR_CANNOT_OPEN_PROFILE       = 1205 :: SystemErrorCode

pattern ERROR_BAD_PROFILE               = 1206 :: SystemErrorCode

pattern ERROR_NOT_CONTAINER             = 1207 :: SystemErrorCode

pattern ERROR_EXTENDED_ERROR            = 1208 :: SystemErrorCode

pattern ERROR_INVALID_GROUPNAME         = 1209 :: SystemErrorCode

pattern ERROR_INVALID_COMPUTERNAME      = 1210 :: SystemErrorCode

pattern ERROR_INVALID_EVENTNAME         = 1211 :: SystemErrorCode

pattern ERROR_INVALID_DOMAINNAME        = 1212 :: SystemErrorCode

pattern ERROR_INVALID_SERVICENAME       = 1213 :: SystemErrorCode

pattern ERROR_INVALID_NETNAME           = 1214 :: SystemErrorCode

pattern ERROR_INVALID_SHARENAME         = 1215 :: SystemErrorCode

pattern ERROR_INVALID_PASSWORDNAME      = 1216 :: SystemErrorCode

pattern ERROR_INVALID_MESSAGENAME       = 1217 :: SystemErrorCode

pattern ERROR_INVALID_MESSAGEDEST       = 1218 :: SystemErrorCode

pattern ERROR_SESSION_CREDENTIAL_CONFLICT= 1219 :: SystemErrorCode

pattern ERROR_REMOTE_SESSION_LIMIT_EXCEEDED= 1220 :: SystemErrorCode

pattern ERROR_DUP_DOMAINNAME            = 1221 :: SystemErrorCode

pattern ERROR_NO_NETWORK                = 1222 :: SystemErrorCode

pattern ERROR_CANCELLED                 = 1223 :: SystemErrorCode

pattern ERROR_USER_MAPPED_FILE          = 1224 :: SystemErrorCode

pattern ERROR_CONNECTION_REFUSED        = 1225 :: SystemErrorCode

pattern ERROR_GRACEFUL_DISCONNECT       = 1226 :: SystemErrorCode

pattern ERROR_ADDRESS_ALREADY_ASSOCIATED= 1227 :: SystemErrorCode

pattern ERROR_ADDRESS_NOT_ASSOCIATED    = 1228 :: SystemErrorCode

pattern ERROR_CONNECTION_INVALID        = 1229 :: SystemErrorCode

pattern ERROR_CONNECTION_ACTIVE         = 1230 :: SystemErrorCode

pattern ERROR_NETWORK_UNREACHABLE       = 1231 :: SystemErrorCode

pattern ERROR_HOST_UNREACHABLE          = 1232 :: SystemErrorCode

pattern ERROR_PROTOCOL_UNREACHABLE      = 1233 :: SystemErrorCode

pattern ERROR_PORT_UNREACHABLE          = 1234 :: SystemErrorCode

pattern ERROR_REQUEST_ABORTED           = 1235 :: SystemErrorCode

pattern ERROR_CONNECTION_ABORTED        = 1236 :: SystemErrorCode

pattern ERROR_RETRY                     = 1237 :: SystemErrorCode

pattern ERROR_CONNECTION_COUNT_LIMIT    = 1238 :: SystemErrorCode

pattern ERROR_LOGIN_TIME_RESTRICTION    = 1239 :: SystemErrorCode

pattern ERROR_LOGIN_WKSTA_RESTRICTION   = 1240 :: SystemErrorCode

pattern ERROR_INCORRECT_ADDRESS         = 1241 :: SystemErrorCode

pattern ERROR_ALREADY_REGISTERED        = 1242 :: SystemErrorCode

pattern ERROR_SERVICE_NOT_FOUND         = 1243 :: SystemErrorCode

pattern ERROR_NOT_AUTHENTICATED         = 1244 :: SystemErrorCode

pattern ERROR_NOT_LOGGED_ON             = 1245 :: SystemErrorCode

pattern ERROR_CONTINUE                  = 1246 :: SystemErrorCode     -- dderror

pattern ERROR_ALREADY_INITIALIZED       = 1247 :: SystemErrorCode

pattern ERROR_NO_MORE_DEVICES           = 1248 :: SystemErrorCode     -- dderror

pattern ERROR_NO_SUCH_SITE              = 1249 :: SystemErrorCode

pattern ERROR_DOMAIN_CONTROLLER_EXISTS  = 1250 :: SystemErrorCode

pattern ERROR_ONLY_IF_CONNECTED         = 1251 :: SystemErrorCode

pattern ERROR_OVERRIDE_NOCHANGES        = 1252 :: SystemErrorCode

pattern ERROR_BAD_USER_PROFILE          = 1253 :: SystemErrorCode

pattern ERROR_NOT_SUPPORTED_ON_SBS      = 1254 :: SystemErrorCode

pattern ERROR_SERVER_SHUTDOWN_IN_PROGRESS= 1255 :: SystemErrorCode

pattern ERROR_HOST_DOWN                 = 1256 :: SystemErrorCode

pattern ERROR_NON_ACCOUNT_SID           = 1257 :: SystemErrorCode

pattern ERROR_NON_DOMAIN_SID            = 1258 :: SystemErrorCode

pattern ERROR_APPHELP_BLOCK             = 1259 :: SystemErrorCode

pattern ERROR_ACCESS_DISABLED_BY_POLICY = 1260 :: SystemErrorCode

pattern ERROR_REG_NAT_CONSUMPTION       = 1261 :: SystemErrorCode

pattern ERROR_CSCSHARE_OFFLINE          = 1262 :: SystemErrorCode

pattern ERROR_PKINIT_FAILURE            = 1263 :: SystemErrorCode

pattern ERROR_SMARTCARD_SUBSYSTEM_FAILURE= 1264 :: SystemErrorCode

pattern ERROR_DOWNGRADE_DETECTED        = 1265 :: SystemErrorCode

pattern ERROR_MACHINE_LOCKED            = 1271 :: SystemErrorCode

pattern ERROR_CALLBACK_SUPPLIED_INVALID_DATA= 1273 :: SystemErrorCode

pattern ERROR_SYNC_FOREGROUND_REFRESH_REQUIRED= 1274 :: SystemErrorCode

pattern ERROR_DRIVER_BLOCKED            = 1275 :: SystemErrorCode

pattern ERROR_INVALID_IMPORT_OF_NON_DLL = 1276 :: SystemErrorCode

pattern ERROR_ACCESS_DISABLED_WEBBLADE  = 1277 :: SystemErrorCode

pattern ERROR_ACCESS_DISABLED_WEBBLADE_TAMPER= 1278 :: SystemErrorCode

pattern ERROR_RECOVERY_FAILURE          = 1279 :: SystemErrorCode

pattern ERROR_ALREADY_FIBER             = 1280 :: SystemErrorCode

pattern ERROR_ALREADY_THREAD            = 1281 :: SystemErrorCode

pattern ERROR_STACK_BUFFER_OVERRUN      = 1282 :: SystemErrorCode

pattern ERROR_PARAMETER_QUOTA_EXCEEDED  = 1283 :: SystemErrorCode

pattern ERROR_DEBUGGER_INACTIVE         = 1284 :: SystemErrorCode

pattern ERROR_DELAY_LOAD_FAILED         = 1285 :: SystemErrorCode

pattern ERROR_VDM_DISALLOWED            = 1286 :: SystemErrorCode

pattern ERROR_UNIDENTIFIED_ERROR        = 1287 :: SystemErrorCode

pattern ERROR_INVALID_CRUNTIME_PARAMETER= 1288 :: SystemErrorCode

pattern ERROR_BEYOND_VDL                = 1289 :: SystemErrorCode

pattern ERROR_INCOMPATIBLE_SERVICE_SID_TYPE= 1290 :: SystemErrorCode

pattern ERROR_DRIVER_PROCESS_TERMINATED = 1291 :: SystemErrorCode

pattern ERROR_IMPLEMENTATION_LIMIT      = 1292 :: SystemErrorCode

pattern ERROR_PROCESS_IS_PROTECTED      = 1293 :: SystemErrorCode

pattern ERROR_SERVICE_NOTIFY_CLIENT_LAGGING= 1294 :: SystemErrorCode

pattern ERROR_DISK_QUOTA_EXCEEDED       = 1295 :: SystemErrorCode

pattern ERROR_CONTENT_BLOCKED           = 1296 :: SystemErrorCode

pattern ERROR_INCOMPATIBLE_SERVICE_PRIVILEGE= 1297 :: SystemErrorCode

pattern ERROR_APP_HANG                  = 1298 :: SystemErrorCode

pattern ERROR_INVALID_LABEL             = 1299 :: SystemErrorCode

pattern ERROR_NOT_ALL_ASSIGNED          = 1300 :: SystemErrorCode

pattern ERROR_SOME_NOT_MAPPED           = 1301 :: SystemErrorCode

pattern ERROR_NO_QUOTAS_FOR_ACCOUNT     = 1302 :: SystemErrorCode

pattern ERROR_LOCAL_USER_SESSION_KEY    = 1303 :: SystemErrorCode

pattern ERROR_NULL_LM_PASSWORD          = 1304 :: SystemErrorCode

pattern ERROR_UNKNOWN_REVISION          = 1305 :: SystemErrorCode

pattern ERROR_REVISION_MISMATCH         = 1306 :: SystemErrorCode

pattern ERROR_INVALID_OWNER             = 1307 :: SystemErrorCode

pattern ERROR_INVALID_PRIMARY_GROUP     = 1308 :: SystemErrorCode

pattern ERROR_NO_IMPERSONATION_TOKEN    = 1309 :: SystemErrorCode

pattern ERROR_CANT_DISABLE_MANDATORY    = 1310 :: SystemErrorCode

pattern ERROR_NO_LOGON_SERVERS          = 1311 :: SystemErrorCode

pattern ERROR_NO_SUCH_LOGON_SESSION     = 1312 :: SystemErrorCode

pattern ERROR_NO_SUCH_PRIVILEGE         = 1313 :: SystemErrorCode

pattern ERROR_PRIVILEGE_NOT_HELD        = 1314 :: SystemErrorCode

pattern ERROR_INVALID_ACCOUNT_NAME      = 1315 :: SystemErrorCode

pattern ERROR_USER_EXISTS               = 1316 :: SystemErrorCode

pattern ERROR_NO_SUCH_USER              = 1317 :: SystemErrorCode

pattern ERROR_GROUP_EXISTS              = 1318 :: SystemErrorCode

pattern ERROR_NO_SUCH_GROUP             = 1319 :: SystemErrorCode

pattern ERROR_MEMBER_IN_GROUP           = 1320 :: SystemErrorCode

pattern ERROR_MEMBER_NOT_IN_GROUP       = 1321 :: SystemErrorCode

pattern ERROR_LAST_ADMIN                = 1322 :: SystemErrorCode

pattern ERROR_WRONG_PASSWORD            = 1323 :: SystemErrorCode

pattern ERROR_ILL_FORMED_PASSWORD       = 1324 :: SystemErrorCode

pattern ERROR_PASSWORD_RESTRICTION      = 1325 :: SystemErrorCode

pattern ERROR_LOGON_FAILURE             = 1326 :: SystemErrorCode

pattern ERROR_ACCOUNT_RESTRICTION       = 1327 :: SystemErrorCode

pattern ERROR_INVALID_LOGON_HOURS       = 1328 :: SystemErrorCode

pattern ERROR_INVALID_WORKSTATION       = 1329 :: SystemErrorCode

pattern ERROR_PASSWORD_EXPIRED          = 1330 :: SystemErrorCode

pattern ERROR_ACCOUNT_DISABLED          = 1331 :: SystemErrorCode

pattern ERROR_NONE_MAPPED               = 1332 :: SystemErrorCode

pattern ERROR_TOO_MANY_LUIDS_REQUESTED  = 1333 :: SystemErrorCode

pattern ERROR_LUIDS_EXHAUSTED           = 1334 :: SystemErrorCode

pattern ERROR_INVALID_SUB_AUTHORITY     = 1335 :: SystemErrorCode

pattern ERROR_INVALID_ACL               = 1336 :: SystemErrorCode

pattern ERROR_INVALID_SID               = 1337 :: SystemErrorCode

pattern ERROR_INVALID_SECURITY_DESCR    = 1338 :: SystemErrorCode

pattern ERROR_BAD_INHERITANCE_ACL       = 1340 :: SystemErrorCode

pattern ERROR_SERVER_DISABLED           = 1341 :: SystemErrorCode

pattern ERROR_SERVER_NOT_DISABLED       = 1342 :: SystemErrorCode

pattern ERROR_INVALID_ID_AUTHORITY      = 1343 :: SystemErrorCode

pattern ERROR_ALLOTTED_SPACE_EXCEEDED   = 1344 :: SystemErrorCode

pattern ERROR_INVALID_GROUP_ATTRIBUTES  = 1345 :: SystemErrorCode

pattern ERROR_BAD_IMPERSONATION_LEVEL   = 1346 :: SystemErrorCode

pattern ERROR_CANT_OPEN_ANONYMOUS       = 1347 :: SystemErrorCode

pattern ERROR_BAD_VALIDATION_CLASS      = 1348 :: SystemErrorCode

pattern ERROR_BAD_TOKEN_TYPE            = 1349 :: SystemErrorCode

pattern ERROR_NO_SECURITY_ON_OBJECT     = 1350 :: SystemErrorCode

pattern ERROR_CANT_ACCESS_DOMAIN_INFO   = 1351 :: SystemErrorCode

pattern ERROR_INVALID_SERVER_STATE      = 1352 :: SystemErrorCode

pattern ERROR_INVALID_DOMAIN_STATE      = 1353 :: SystemErrorCode

pattern ERROR_INVALID_DOMAIN_ROLE       = 1354 :: SystemErrorCode

pattern ERROR_NO_SUCH_DOMAIN            = 1355 :: SystemErrorCode

pattern ERROR_DOMAIN_EXISTS             = 1356 :: SystemErrorCode

pattern ERROR_DOMAIN_LIMIT_EXCEEDED     = 1357 :: SystemErrorCode

pattern ERROR_INTERNAL_DB_CORRUPTION    = 1358 :: SystemErrorCode

pattern ERROR_INTERNAL_ERROR            = 1359 :: SystemErrorCode

pattern ERROR_GENERIC_NOT_MAPPED        = 1360 :: SystemErrorCode

pattern ERROR_BAD_DESCRIPTOR_FORMAT     = 1361 :: SystemErrorCode

pattern ERROR_NOT_LOGON_PROCESS         = 1362 :: SystemErrorCode

pattern ERROR_LOGON_SESSION_EXISTS      = 1363 :: SystemErrorCode

pattern ERROR_NO_SUCH_PACKAGE           = 1364 :: SystemErrorCode

pattern ERROR_BAD_LOGON_SESSION_STATE   = 1365 :: SystemErrorCode

pattern ERROR_LOGON_SESSION_COLLISION   = 1366 :: SystemErrorCode

pattern ERROR_INVALID_LOGON_TYPE        = 1367 :: SystemErrorCode

pattern ERROR_CANNOT_IMPERSONATE        = 1368 :: SystemErrorCode

pattern ERROR_RXACT_INVALID_STATE       = 1369 :: SystemErrorCode

pattern ERROR_RXACT_COMMIT_FAILURE      = 1370 :: SystemErrorCode

pattern ERROR_SPECIAL_ACCOUNT           = 1371 :: SystemErrorCode

pattern ERROR_SPECIAL_GROUP             = 1372 :: SystemErrorCode

pattern ERROR_SPECIAL_USER              = 1373 :: SystemErrorCode

pattern ERROR_MEMBERS_PRIMARY_GROUP     = 1374 :: SystemErrorCode

pattern ERROR_TOKEN_ALREADY_IN_USE      = 1375 :: SystemErrorCode

pattern ERROR_NO_SUCH_ALIAS             = 1376 :: SystemErrorCode

pattern ERROR_MEMBER_NOT_IN_ALIAS       = 1377 :: SystemErrorCode

pattern ERROR_MEMBER_IN_ALIAS           = 1378 :: SystemErrorCode

pattern ERROR_ALIAS_EXISTS              = 1379 :: SystemErrorCode

pattern ERROR_LOGON_NOT_GRANTED         = 1380 :: SystemErrorCode

pattern ERROR_TOO_MANY_SECRETS          = 1381 :: SystemErrorCode

pattern ERROR_SECRET_TOO_LONG           = 1382 :: SystemErrorCode

pattern ERROR_INTERNAL_DB_ERROR         = 1383 :: SystemErrorCode

pattern ERROR_TOO_MANY_CONTEXT_IDS      = 1384 :: SystemErrorCode

pattern ERROR_LOGON_TYPE_NOT_GRANTED    = 1385 :: SystemErrorCode

pattern ERROR_NT_CROSS_ENCRYPTION_REQUIRED= 1386 :: SystemErrorCode

pattern ERROR_NO_SUCH_MEMBER            = 1387 :: SystemErrorCode

pattern ERROR_INVALID_MEMBER            = 1388 :: SystemErrorCode

pattern ERROR_TOO_MANY_SIDS             = 1389 :: SystemErrorCode

pattern ERROR_LM_CROSS_ENCRYPTION_REQUIRED= 1390 :: SystemErrorCode

pattern ERROR_NO_INHERITANCE            = 1391 :: SystemErrorCode

pattern ERROR_FILE_CORRUPT              = 1392 :: SystemErrorCode

pattern ERROR_DISK_CORRUPT              = 1393 :: SystemErrorCode

pattern ERROR_NO_USER_SESSION_KEY       = 1394 :: SystemErrorCode

pattern ERROR_LICENSE_QUOTA_EXCEEDED    = 1395 :: SystemErrorCode

pattern ERROR_WRONG_TARGET_NAME         = 1396 :: SystemErrorCode

pattern ERROR_MUTUAL_AUTH_FAILED        = 1397 :: SystemErrorCode

pattern ERROR_TIME_SKEW                 = 1398 :: SystemErrorCode

pattern ERROR_CURRENT_DOMAIN_NOT_ALLOWED= 1399 :: SystemErrorCode

pattern ERROR_INVALID_WINDOW_HANDLE     = 1400 :: SystemErrorCode

pattern ERROR_INVALID_MENU_HANDLE       = 1401 :: SystemErrorCode

pattern ERROR_INVALID_CURSOR_HANDLE     = 1402 :: SystemErrorCode

pattern ERROR_INVALID_ACCEL_HANDLE      = 1403 :: SystemErrorCode

pattern ERROR_INVALID_HOOK_HANDLE       = 1404 :: SystemErrorCode

pattern ERROR_INVALID_DWP_HANDLE        = 1405 :: SystemErrorCode

pattern ERROR_TLW_WITH_WSCHILD          = 1406 :: SystemErrorCode

pattern ERROR_CANNOT_FIND_WND_CLASS     = 1407 :: SystemErrorCode

pattern ERROR_WINDOW_OF_OTHER_THREAD    = 1408 :: SystemErrorCode

pattern ERROR_HOTKEY_ALREADY_REGISTERED = 1409 :: SystemErrorCode

pattern ERROR_CLASS_ALREADY_EXISTS      = 1410 :: SystemErrorCode

pattern ERROR_CLASS_DOES_NOT_EXIST      = 1411 :: SystemErrorCode

pattern ERROR_CLASS_HAS_WINDOWS         = 1412 :: SystemErrorCode

pattern ERROR_INVALID_INDEX             = 1413 :: SystemErrorCode

pattern ERROR_INVALID_ICON_HANDLE       = 1414 :: SystemErrorCode

pattern ERROR_PRIVATE_DIALOG_INDEX      = 1415 :: SystemErrorCode

pattern ERROR_LISTBOX_ID_NOT_FOUND      = 1416 :: SystemErrorCode

pattern ERROR_NO_WILDCARD_CHARACTERS    = 1417 :: SystemErrorCode

pattern ERROR_CLIPBOARD_NOT_OPEN        = 1418 :: SystemErrorCode

pattern ERROR_HOTKEY_NOT_REGISTERED     = 1419 :: SystemErrorCode

pattern ERROR_WINDOW_NOT_DIALOG         = 1420 :: SystemErrorCode

pattern ERROR_CONTROL_ID_NOT_FOUND      = 1421 :: SystemErrorCode

pattern ERROR_INVALID_COMBOBOX_MESSAGE  = 1422 :: SystemErrorCode

pattern ERROR_WINDOW_NOT_COMBOBOX       = 1423 :: SystemErrorCode

pattern ERROR_INVALID_EDIT_HEIGHT       = 1424 :: SystemErrorCode

pattern ERROR_DC_NOT_FOUND              = 1425 :: SystemErrorCode

pattern ERROR_INVALID_HOOK_FILTER       = 1426 :: SystemErrorCode

pattern ERROR_INVALID_FILTER_PROC       = 1427 :: SystemErrorCode

pattern ERROR_HOOK_NEEDS_HMOD           = 1428 :: SystemErrorCode

pattern ERROR_GLOBAL_ONLY_HOOK          = 1429 :: SystemErrorCode

pattern ERROR_JOURNAL_HOOK_SET          = 1430 :: SystemErrorCode

pattern ERROR_HOOK_NOT_INSTALLED        = 1431 :: SystemErrorCode

pattern ERROR_INVALID_LB_MESSAGE        = 1432 :: SystemErrorCode

pattern ERROR_SETCOUNT_ON_BAD_LB        = 1433 :: SystemErrorCode

pattern ERROR_LB_WITHOUT_TABSTOPS       = 1434 :: SystemErrorCode

pattern ERROR_DESTROY_OBJECT_OF_OTHER_THREAD= 1435 :: SystemErrorCode

pattern ERROR_CHILD_WINDOW_MENU         = 1436 :: SystemErrorCode

pattern ERROR_NO_SYSTEM_MENU            = 1437 :: SystemErrorCode

pattern ERROR_INVALID_MSGBOX_STYLE      = 1438 :: SystemErrorCode

pattern ERROR_INVALID_SPI_VALUE         = 1439 :: SystemErrorCode

pattern ERROR_SCREEN_ALREADY_LOCKED     = 1440 :: SystemErrorCode

pattern ERROR_HWNDS_HAVE_DIFF_PARENT    = 1441 :: SystemErrorCode

pattern ERROR_NOT_CHILD_WINDOW          = 1442 :: SystemErrorCode

pattern ERROR_INVALID_GW_COMMAND        = 1443 :: SystemErrorCode

pattern ERROR_INVALID_THREAD_ID         = 1444 :: SystemErrorCode

pattern ERROR_NON_MDICHILD_WINDOW       = 1445 :: SystemErrorCode

pattern ERROR_POPUP_ALREADY_ACTIVE      = 1446 :: SystemErrorCode

pattern ERROR_NO_SCROLLBARS             = 1447 :: SystemErrorCode

pattern ERROR_INVALID_SCROLLBAR_RANGE   = 1448 :: SystemErrorCode

pattern ERROR_INVALID_SHOWWIN_COMMAND   = 1449 :: SystemErrorCode

pattern ERROR_NO_SYSTEM_RESOURCES       = 1450 :: SystemErrorCode

pattern ERROR_NONPAGED_SYSTEM_RESOURCES = 1451 :: SystemErrorCode

pattern ERROR_PAGED_SYSTEM_RESOURCES    = 1452 :: SystemErrorCode

pattern ERROR_WORKING_SET_QUOTA         = 1453 :: SystemErrorCode

pattern ERROR_PAGEFILE_QUOTA            = 1454 :: SystemErrorCode

pattern ERROR_COMMITMENT_LIMIT          = 1455 :: SystemErrorCode

pattern ERROR_MENU_ITEM_NOT_FOUND       = 1456 :: SystemErrorCode

pattern ERROR_INVALID_KEYBOARD_HANDLE   = 1457 :: SystemErrorCode

pattern ERROR_HOOK_TYPE_NOT_ALLOWED     = 1458 :: SystemErrorCode

pattern ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION= 1459 :: SystemErrorCode

pattern ERROR_TIMEOUT                   = 1460 :: SystemErrorCode

pattern ERROR_INVALID_MONITOR_HANDLE    = 1461 :: SystemErrorCode

pattern ERROR_INCORRECT_SIZE            = 1462 :: SystemErrorCode

pattern ERROR_SYMLINK_CLASS_DISABLED    = 1463 :: SystemErrorCode

pattern ERROR_SYMLINK_NOT_SUPPORTED     = 1464 :: SystemErrorCode

pattern ERROR_XML_PARSE_ERROR           = 1465 :: SystemErrorCode

pattern ERROR_XMLDSIG_ERROR             = 1466 :: SystemErrorCode

pattern ERROR_RESTART_APPLICATION       = 1467 :: SystemErrorCode

pattern ERROR_WRONG_COMPARTMENT         = 1468 :: SystemErrorCode

pattern ERROR_AUTHIP_FAILURE            = 1469 :: SystemErrorCode

pattern ERROR_NO_NVRAM_RESOURCES        = 1470 :: SystemErrorCode

pattern ERROR_NOT_GUI_PROCESS           = 1471 :: SystemErrorCode

pattern ERROR_EVENTLOG_FILE_CORRUPT     = 1500 :: SystemErrorCode

pattern ERROR_EVENTLOG_CANT_START       = 1501 :: SystemErrorCode

pattern ERROR_LOG_FILE_FULL             = 1502 :: SystemErrorCode

pattern ERROR_EVENTLOG_FILE_CHANGED     = 1503 :: SystemErrorCode

pattern ERROR_INVALID_TASK_NAME         = 1550 :: SystemErrorCode

pattern ERROR_INVALID_TASK_INDEX        = 1551 :: SystemErrorCode

pattern ERROR_THREAD_ALREADY_IN_TASK    = 1552 :: SystemErrorCode

pattern ERROR_INSTALL_SERVICE_FAILURE   = 1601 :: SystemErrorCode

pattern ERROR_INSTALL_USEREXIT          = 1602 :: SystemErrorCode

pattern ERROR_INSTALL_FAILURE           = 1603 :: SystemErrorCode

pattern ERROR_INSTALL_SUSPEND           = 1604 :: SystemErrorCode

pattern ERROR_UNKNOWN_PRODUCT           = 1605 :: SystemErrorCode

pattern ERROR_UNKNOWN_FEATURE           = 1606 :: SystemErrorCode

pattern ERROR_UNKNOWN_COMPONENT         = 1607 :: SystemErrorCode

pattern ERROR_UNKNOWN_PROPERTY          = 1608 :: SystemErrorCode

pattern ERROR_INVALID_HANDLE_STATE      = 1609 :: SystemErrorCode

pattern ERROR_BAD_CONFIGURATION         = 1610 :: SystemErrorCode

pattern ERROR_INDEX_ABSENT              = 1611 :: SystemErrorCode

pattern ERROR_INSTALL_SOURCE_ABSENT     = 1612 :: SystemErrorCode

pattern ERROR_INSTALL_PACKAGE_VERSION   = 1613 :: SystemErrorCode

pattern ERROR_PRODUCT_UNINSTALLED       = 1614 :: SystemErrorCode

pattern ERROR_BAD_QUERY_SYNTAX          = 1615 :: SystemErrorCode

pattern ERROR_INVALID_FIELD             = 1616 :: SystemErrorCode

pattern ERROR_DEVICE_REMOVED            = 1617 :: SystemErrorCode

pattern ERROR_INSTALL_ALREADY_RUNNING   = 1618 :: SystemErrorCode

pattern ERROR_INSTALL_PACKAGE_OPEN_FAILED= 1619 :: SystemErrorCode

pattern ERROR_INSTALL_PACKAGE_INVALID   = 1620 :: SystemErrorCode

pattern ERROR_INSTALL_UI_FAILURE        = 1621 :: SystemErrorCode

pattern ERROR_INSTALL_LOG_FAILURE       = 1622 :: SystemErrorCode

pattern ERROR_INSTALL_LANGUAGE_UNSUPPORTED= 1623 :: SystemErrorCode

pattern ERROR_INSTALL_TRANSFORM_FAILURE = 1624 :: SystemErrorCode

pattern ERROR_INSTALL_PACKAGE_REJECTED  = 1625 :: SystemErrorCode

pattern ERROR_FUNCTION_NOT_CALLED       = 1626 :: SystemErrorCode

pattern ERROR_FUNCTION_FAILED           = 1627 :: SystemErrorCode

pattern ERROR_INVALID_TABLE             = 1628 :: SystemErrorCode

pattern ERROR_DATATYPE_MISMATCH         = 1629 :: SystemErrorCode

pattern ERROR_UNSUPPORTED_TYPE          = 1630 :: SystemErrorCode

pattern ERROR_CREATE_FAILED             = 1631 :: SystemErrorCode

pattern ERROR_INSTALL_TEMP_UNWRITABLE   = 1632 :: SystemErrorCode

pattern ERROR_INSTALL_PLATFORM_UNSUPPORTED= 1633 :: SystemErrorCode

pattern ERROR_INSTALL_NOTUSED           = 1634 :: SystemErrorCode

pattern ERROR_PATCH_PACKAGE_OPEN_FAILED = 1635 :: SystemErrorCode

pattern ERROR_PATCH_PACKAGE_INVALID     = 1636 :: SystemErrorCode

pattern ERROR_PATCH_PACKAGE_UNSUPPORTED = 1637 :: SystemErrorCode

pattern ERROR_PRODUCT_VERSION           = 1638 :: SystemErrorCode

pattern ERROR_INVALID_COMMAND_LINE      = 1639 :: SystemErrorCode

pattern ERROR_INSTALL_REMOTE_DISALLOWED = 1640 :: SystemErrorCode

pattern ERROR_SUCCESS_REBOOT_INITIATED  = 1641 :: SystemErrorCode

pattern ERROR_PATCH_TARGET_NOT_FOUND    = 1642 :: SystemErrorCode

pattern ERROR_PATCH_PACKAGE_REJECTED    = 1643 :: SystemErrorCode

pattern ERROR_INSTALL_TRANSFORM_REJECTED= 1644 :: SystemErrorCode

pattern ERROR_INSTALL_REMOTE_PROHIBITED = 1645 :: SystemErrorCode

pattern ERROR_PATCH_REMOVAL_UNSUPPORTED = 1646 :: SystemErrorCode

pattern ERROR_UNKNOWN_PATCH             = 1647 :: SystemErrorCode

pattern ERROR_PATCH_NO_SEQUENCE         = 1648 :: SystemErrorCode

pattern ERROR_PATCH_REMOVAL_DISALLOWED  = 1649 :: SystemErrorCode

pattern ERROR_INVALID_PATCH_XML         = 1650 :: SystemErrorCode

pattern ERROR_PATCH_MANAGED_ADVERTISED_PRODUCT= 1651 :: SystemErrorCode

pattern ERROR_INSTALL_SERVICE_SAFEBOOT  = 1652 :: SystemErrorCode

pattern ERROR_FAIL_FAST_EXCEPTION       = 1653 :: SystemErrorCode

pattern ERROR_INSTALL_REJECTED          = 1654 :: SystemErrorCode

pattern ERROR_DYNAMIC_CODE_BLOCKED      = 1655 :: SystemErrorCode

pattern ERROR_INVALID_USER_BUFFER       = 1784 :: SystemErrorCode

pattern ERROR_UNRECOGNIZED_MEDIA        = 1785 :: SystemErrorCode

pattern ERROR_NO_TRUST_LSA_SECRET       = 1786 :: SystemErrorCode

pattern ERROR_NO_TRUST_SAM_ACCOUNT      = 1787 :: SystemErrorCode

pattern ERROR_TRUSTED_DOMAIN_FAILURE    = 1788 :: SystemErrorCode

pattern ERROR_TRUSTED_RELATIONSHIP_FAILURE= 1789 :: SystemErrorCode

pattern ERROR_TRUST_FAILURE             = 1790 :: SystemErrorCode

pattern ERROR_NETLOGON_NOT_STARTED      = 1792 :: SystemErrorCode

pattern ERROR_ACCOUNT_EXPIRED           = 1793 :: SystemErrorCode

pattern ERROR_REDIRECTOR_HAS_OPEN_HANDLES= 1794 :: SystemErrorCode

pattern ERROR_PRINTER_DRIVER_ALREADY_INSTALLED= 1795 :: SystemErrorCode

pattern ERROR_UNKNOWN_PORT              = 1796 :: SystemErrorCode

pattern ERROR_UNKNOWN_PRINTER_DRIVER    = 1797 :: SystemErrorCode

pattern ERROR_UNKNOWN_PRINTPROCESSOR    = 1798 :: SystemErrorCode

pattern ERROR_INVALID_SEPARATOR_FILE    = 1799 :: SystemErrorCode

pattern ERROR_INVALID_PRIORITY          = 1800 :: SystemErrorCode

pattern ERROR_INVALID_PRINTER_NAME      = 1801 :: SystemErrorCode

pattern ERROR_PRINTER_ALREADY_EXISTS    = 1802 :: SystemErrorCode

pattern ERROR_INVALID_PRINTER_COMMAND   = 1803 :: SystemErrorCode

pattern ERROR_INVALID_DATATYPE          = 1804 :: SystemErrorCode

pattern ERROR_INVALID_ENVIRONMENT       = 1805 :: SystemErrorCode

pattern ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT= 1807 :: SystemErrorCode

pattern ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT= 1808 :: SystemErrorCode

pattern ERROR_NOLOGON_SERVER_TRUST_ACCOUNT= 1809 :: SystemErrorCode

pattern ERROR_DOMAIN_TRUST_INCONSISTENT = 1810 :: SystemErrorCode

pattern ERROR_SERVER_HAS_OPEN_HANDLES   = 1811 :: SystemErrorCode

pattern ERROR_RESOURCE_DATA_NOT_FOUND   = 1812 :: SystemErrorCode

pattern ERROR_RESOURCE_TYPE_NOT_FOUND   = 1813 :: SystemErrorCode

pattern ERROR_RESOURCE_NAME_NOT_FOUND   = 1814 :: SystemErrorCode

pattern ERROR_RESOURCE_LANG_NOT_FOUND   = 1815 :: SystemErrorCode

pattern ERROR_NOT_ENOUGH_QUOTA          = 1816 :: SystemErrorCode

pattern ERROR_INVALID_TIME              = 1901 :: SystemErrorCode

pattern ERROR_INVALID_FORM_NAME         = 1902 :: SystemErrorCode

pattern ERROR_INVALID_FORM_SIZE         = 1903 :: SystemErrorCode

pattern ERROR_ALREADY_WAITING           = 1904 :: SystemErrorCode

pattern ERROR_PRINTER_DELETED           = 1905 :: SystemErrorCode

pattern ERROR_INVALID_PRINTER_STATE     = 1906 :: SystemErrorCode

pattern ERROR_PASSWORD_MUST_CHANGE      = 1907 :: SystemErrorCode

pattern ERROR_DOMAIN_CONTROLLER_NOT_FOUND= 1908 :: SystemErrorCode

pattern ERROR_ACCOUNT_LOCKED_OUT        = 1909 :: SystemErrorCode

pattern ERROR_NO_SITENAME               = 1919 :: SystemErrorCode

pattern ERROR_CANT_ACCESS_FILE          = 1920 :: SystemErrorCode

pattern ERROR_CANT_RESOLVE_FILENAME     = 1921 :: SystemErrorCode

pattern ERROR_KM_DRIVER_BLOCKED         = 1930 :: SystemErrorCode

pattern ERROR_CONTEXT_EXPIRED           = 1931 :: SystemErrorCode

pattern ERROR_PER_USER_TRUST_QUOTA_EXCEEDED= 1932 :: SystemErrorCode

pattern ERROR_ALL_USER_TRUST_QUOTA_EXCEEDED= 1933 :: SystemErrorCode

pattern ERROR_USER_DELETE_TRUST_QUOTA_EXCEEDED= 1934 :: SystemErrorCode

pattern ERROR_AUTHENTICATION_FIREWALL_FAILED= 1935 :: SystemErrorCode

pattern ERROR_REMOTE_PRINT_CONNECTIONS_BLOCKED= 1936 :: SystemErrorCode

pattern ERROR_NTLM_BLOCKED              = 1937 :: SystemErrorCode

pattern ERROR_PASSWORD_CHANGE_REQUIRED  = 1938 :: SystemErrorCode

pattern ERROR_INVALID_PIXEL_FORMAT      = 2000 :: SystemErrorCode

pattern ERROR_BAD_DRIVER                = 2001 :: SystemErrorCode

pattern ERROR_INVALID_WINDOW_STYLE      = 2002 :: SystemErrorCode

pattern ERROR_METAFILE_NOT_SUPPORTED    = 2003 :: SystemErrorCode

pattern ERROR_TRANSFORM_NOT_SUPPORTED   = 2004 :: SystemErrorCode

pattern ERROR_CLIPPING_NOT_SUPPORTED    = 2005 :: SystemErrorCode

pattern ERROR_INVALID_CMM               = 2010 :: SystemErrorCode

pattern ERROR_INVALID_PROFILE           = 2011 :: SystemErrorCode

pattern ERROR_TAG_NOT_FOUND             = 2012 :: SystemErrorCode

pattern ERROR_TAG_NOT_PRESENT           = 2013 :: SystemErrorCode

pattern ERROR_DUPLICATE_TAG             = 2014 :: SystemErrorCode

pattern ERROR_PROFILE_NOT_ASSOCIATED_WITH_DEVICE= 2015 :: SystemErrorCode

pattern ERROR_PROFILE_NOT_FOUND         = 2016 :: SystemErrorCode

pattern ERROR_INVALID_COLORSPACE        = 2017 :: SystemErrorCode

pattern ERROR_ICM_NOT_ENABLED           = 2018 :: SystemErrorCode

pattern ERROR_DELETING_ICM_XFORM        = 2019 :: SystemErrorCode

pattern ERROR_INVALID_TRANSFORM         = 2020 :: SystemErrorCode

pattern ERROR_COLORSPACE_MISMATCH       = 2021 :: SystemErrorCode

pattern ERROR_INVALID_COLORINDEX        = 2022 :: SystemErrorCode

pattern ERROR_PROFILE_DOES_NOT_MATCH_DEVICE= 2023 :: SystemErrorCode

pattern ERROR_CONNECTED_OTHER_PASSWORD  = 2108 :: SystemErrorCode

pattern ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT= 2109 :: SystemErrorCode

pattern ERROR_BAD_USERNAME              = 2202 :: SystemErrorCode

pattern ERROR_NOT_CONNECTED             = 2250 :: SystemErrorCode

pattern ERROR_OPEN_FILES                = 2401 :: SystemErrorCode

pattern ERROR_ACTIVE_CONNECTIONS        = 2402 :: SystemErrorCode

pattern ERROR_DEVICE_IN_USE             = 2404 :: SystemErrorCode

pattern ERROR_UNKNOWN_PRINT_MONITOR     = 3000 :: SystemErrorCode

pattern ERROR_PRINTER_DRIVER_IN_USE     = 3001 :: SystemErrorCode

pattern ERROR_SPOOL_FILE_NOT_FOUND      = 3002 :: SystemErrorCode

pattern ERROR_SPL_NO_STARTDOC           = 3003 :: SystemErrorCode

pattern ERROR_SPL_NO_ADDJOB             = 3004 :: SystemErrorCode

pattern ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED= 3005 :: SystemErrorCode

pattern ERROR_PRINT_MONITOR_ALREADY_INSTALLED= 3006 :: SystemErrorCode

pattern ERROR_INVALID_PRINT_MONITOR     = 3007 :: SystemErrorCode

pattern ERROR_PRINT_MONITOR_IN_USE      = 3008 :: SystemErrorCode

pattern ERROR_PRINTER_HAS_JOBS_QUEUED   = 3009 :: SystemErrorCode

pattern ERROR_SUCCESS_REBOOT_REQUIRED   = 3010 :: SystemErrorCode

pattern ERROR_SUCCESS_RESTART_REQUIRED  = 3011 :: SystemErrorCode

pattern ERROR_PRINTER_NOT_FOUND         = 3012 :: SystemErrorCode

pattern ERROR_PRINTER_DRIVER_WARNED     = 3013 :: SystemErrorCode

pattern ERROR_PRINTER_DRIVER_BLOCKED    = 3014 :: SystemErrorCode

pattern ERROR_PRINTER_DRIVER_PACKAGE_IN_USE= 3015 :: SystemErrorCode

pattern ERROR_CORE_DRIVER_PACKAGE_NOT_FOUND= 3016 :: SystemErrorCode

pattern ERROR_FAIL_REBOOT_REQUIRED      = 3017 :: SystemErrorCode

pattern ERROR_FAIL_REBOOT_INITIATED     = 3018 :: SystemErrorCode

pattern ERROR_PRINTER_DRIVER_DOWNLOAD_NEEDED= 3019 :: SystemErrorCode

pattern ERROR_PRINT_JOB_RESTART_REQUIRED= 3020 :: SystemErrorCode

pattern ERROR_INVALID_PRINTER_DRIVER_MANIFEST= 3021 :: SystemErrorCode

pattern ERROR_PRINTER_NOT_SHAREABLE     = 3022 :: SystemErrorCode

pattern ERROR_REQUEST_PAUSED            = 3050 :: SystemErrorCode

pattern ERROR_IO_REISSUE_AS_CACHED      = 3950 :: SystemErrorCode

pattern ERROR_WINS_INTERNAL             = 4000 :: SystemErrorCode

pattern ERROR_CAN_NOT_DEL_LOCAL_WINS    = 4001 :: SystemErrorCode

pattern ERROR_STATIC_INIT               = 4002 :: SystemErrorCode

pattern ERROR_INC_BACKUP                = 4003 :: SystemErrorCode

pattern ERROR_FULL_BACKUP               = 4004 :: SystemErrorCode

pattern ERROR_REC_NON_EXISTENT          = 4005 :: SystemErrorCode

pattern ERROR_RPL_NOT_ALLOWED           = 4006 :: SystemErrorCode

pattern ERROR_DHCP_ADDRESS_CONFLICT     = 4100 :: SystemErrorCode

pattern ERROR_WMI_GUID_NOT_FOUND        = 4200 :: SystemErrorCode

pattern ERROR_WMI_INSTANCE_NOT_FOUND    = 4201 :: SystemErrorCode

pattern ERROR_WMI_ITEMID_NOT_FOUND      = 4202 :: SystemErrorCode

pattern ERROR_WMI_TRY_AGAIN             = 4203 :: SystemErrorCode

pattern ERROR_WMI_DP_NOT_FOUND          = 4204 :: SystemErrorCode

pattern ERROR_WMI_UNRESOLVED_INSTANCE_REF= 4205 :: SystemErrorCode

pattern ERROR_WMI_ALREADY_ENABLED       = 4206 :: SystemErrorCode

pattern ERROR_WMI_GUID_DISCONNECTED     = 4207 :: SystemErrorCode

pattern ERROR_WMI_SERVER_UNAVAILABLE    = 4208 :: SystemErrorCode

pattern ERROR_WMI_DP_FAILED             = 4209 :: SystemErrorCode

pattern ERROR_WMI_INVALID_MOF           = 4210 :: SystemErrorCode

pattern ERROR_WMI_INVALID_REGINFO       = 4211 :: SystemErrorCode

pattern ERROR_WMI_ALREADY_DISABLED      = 4212 :: SystemErrorCode

pattern ERROR_WMI_READ_ONLY             = 4213 :: SystemErrorCode

pattern ERROR_WMI_SET_FAILURE           = 4214 :: SystemErrorCode

pattern ERROR_NOT_APPCONTAINER          = 4250 :: SystemErrorCode

pattern ERROR_APPCONTAINER_REQUIRED     = 4251 :: SystemErrorCode

pattern ERROR_NOT_SUPPORTED_IN_APPCONTAINER= 4252 :: SystemErrorCode

pattern ERROR_INVALID_PACKAGE_SID_LENGTH= 4253 :: SystemErrorCode

pattern ERROR_INVALID_MEDIA             = 4300 :: SystemErrorCode

pattern ERROR_INVALID_LIBRARY           = 4301 :: SystemErrorCode

pattern ERROR_INVALID_MEDIA_POOL        = 4302 :: SystemErrorCode

pattern ERROR_DRIVE_MEDIA_MISMATCH      = 4303 :: SystemErrorCode

pattern ERROR_MEDIA_OFFLINE             = 4304 :: SystemErrorCode

pattern ERROR_LIBRARY_OFFLINE           = 4305 :: SystemErrorCode

pattern ERROR_EMPTY                     = 4306 :: SystemErrorCode

pattern ERROR_NOT_EMPTY                 = 4307 :: SystemErrorCode

pattern ERROR_MEDIA_UNAVAILABLE         = 4308 :: SystemErrorCode

pattern ERROR_RESOURCE_DISABLED         = 4309 :: SystemErrorCode

pattern ERROR_INVALID_CLEANER           = 4310 :: SystemErrorCode

pattern ERROR_UNABLE_TO_CLEAN           = 4311 :: SystemErrorCode

pattern ERROR_OBJECT_NOT_FOUND          = 4312 :: SystemErrorCode

pattern ERROR_DATABASE_FAILURE          = 4313 :: SystemErrorCode

pattern ERROR_DATABASE_FULL             = 4314 :: SystemErrorCode

pattern ERROR_MEDIA_INCOMPATIBLE        = 4315 :: SystemErrorCode

pattern ERROR_RESOURCE_NOT_PRESENT      = 4316 :: SystemErrorCode

pattern ERROR_INVALID_OPERATION         = 4317 :: SystemErrorCode

pattern ERROR_MEDIA_NOT_AVAILABLE       = 4318 :: SystemErrorCode

pattern ERROR_DEVICE_NOT_AVAILABLE      = 4319 :: SystemErrorCode

pattern ERROR_REQUEST_REFUSED           = 4320 :: SystemErrorCode

pattern ERROR_INVALID_DRIVE_OBJECT      = 4321 :: SystemErrorCode

pattern ERROR_LIBRARY_FULL              = 4322 :: SystemErrorCode

pattern ERROR_MEDIUM_NOT_ACCESSIBLE     = 4323 :: SystemErrorCode

pattern ERROR_UNABLE_TO_LOAD_MEDIUM     = 4324 :: SystemErrorCode

pattern ERROR_UNABLE_TO_INVENTORY_DRIVE = 4325 :: SystemErrorCode

pattern ERROR_UNABLE_TO_INVENTORY_SLOT  = 4326 :: SystemErrorCode

pattern ERROR_UNABLE_TO_INVENTORY_TRANSPORT= 4327 :: SystemErrorCode

pattern ERROR_TRANSPORT_FULL            = 4328 :: SystemErrorCode

pattern ERROR_CONTROLLING_IEPORT        = 4329 :: SystemErrorCode

pattern ERROR_UNABLE_TO_EJECT_MOUNTED_MEDIA= 4330 :: SystemErrorCode

pattern ERROR_CLEANER_SLOT_SET          = 4331 :: SystemErrorCode

pattern ERROR_CLEANER_SLOT_NOT_SET      = 4332 :: SystemErrorCode

pattern ERROR_CLEANER_CARTRIDGE_SPENT   = 4333 :: SystemErrorCode

pattern ERROR_UNEXPECTED_OMID           = 4334 :: SystemErrorCode

pattern ERROR_CANT_DELETE_LAST_ITEM     = 4335 :: SystemErrorCode

pattern ERROR_MESSAGE_EXCEEDS_MAX_SIZE  = 4336 :: SystemErrorCode

pattern ERROR_VOLUME_CONTAINS_SYS_FILES = 4337 :: SystemErrorCode

pattern ERROR_INDIGENOUS_TYPE           = 4338 :: SystemErrorCode

pattern ERROR_NO_SUPPORTING_DRIVES      = 4339 :: SystemErrorCode

pattern ERROR_CLEANER_CARTRIDGE_INSTALLED= 4340 :: SystemErrorCode

pattern ERROR_IEPORT_FULL               = 4341 :: SystemErrorCode

pattern ERROR_FILE_OFFLINE              = 4350 :: SystemErrorCode

pattern ERROR_REMOTE_STORAGE_NOT_ACTIVE = 4351 :: SystemErrorCode

pattern ERROR_REMOTE_STORAGE_MEDIA_ERROR= 4352 :: SystemErrorCode

pattern ERROR_NOT_A_REPARSE_POINT       = 4390 :: SystemErrorCode

pattern ERROR_REPARSE_ATTRIBUTE_CONFLICT= 4391 :: SystemErrorCode

pattern ERROR_INVALID_REPARSE_DATA      = 4392 :: SystemErrorCode

pattern ERROR_REPARSE_TAG_INVALID       = 4393 :: SystemErrorCode

pattern ERROR_REPARSE_TAG_MISMATCH      = 4394 :: SystemErrorCode

pattern ERROR_APP_DATA_NOT_FOUND        = 4400 :: SystemErrorCode

pattern ERROR_APP_DATA_EXPIRED          = 4401 :: SystemErrorCode

pattern ERROR_APP_DATA_CORRUPT          = 4402 :: SystemErrorCode

pattern ERROR_APP_DATA_LIMIT_EXCEEDED   = 4403 :: SystemErrorCode

pattern ERROR_APP_DATA_REBOOT_REQUIRED  = 4404 :: SystemErrorCode

pattern ERROR_SECUREBOOT_ROLLBACK_DETECTED= 4420 :: SystemErrorCode

pattern ERROR_SECUREBOOT_POLICY_VIOLATION= 4421 :: SystemErrorCode

pattern ERROR_SECUREBOOT_INVALID_POLICY = 4422 :: SystemErrorCode

pattern ERROR_SECUREBOOT_POLICY_PUBLISHER_NOT_FOUND= 4423 :: SystemErrorCode

pattern ERROR_SECUREBOOT_POLICY_NOT_SIGNED= 4424 :: SystemErrorCode

pattern ERROR_SECUREBOOT_NOT_ENABLED    = 4425 :: SystemErrorCode

pattern ERROR_SECUREBOOT_FILE_REPLACED  = 4426 :: SystemErrorCode

pattern ERROR_OFFLOAD_READ_FLT_NOT_SUPPORTED= 4440 :: SystemErrorCode

pattern ERROR_OFFLOAD_WRITE_FLT_NOT_SUPPORTED= 4441 :: SystemErrorCode

pattern ERROR_OFFLOAD_READ_FILE_NOT_SUPPORTED= 4442 :: SystemErrorCode

pattern ERROR_OFFLOAD_WRITE_FILE_NOT_SUPPORTED= 4443 :: SystemErrorCode

pattern ERROR_VOLUME_NOT_SIS_ENABLED    = 4500 :: SystemErrorCode

pattern ERROR_DEPENDENT_RESOURCE_EXISTS = 5001 :: SystemErrorCode

pattern ERROR_DEPENDENCY_NOT_FOUND      = 5002 :: SystemErrorCode

pattern ERROR_DEPENDENCY_ALREADY_EXISTS = 5003 :: SystemErrorCode

pattern ERROR_RESOURCE_NOT_ONLINE       = 5004 :: SystemErrorCode

pattern ERROR_HOST_NODE_NOT_AVAILABLE   = 5005 :: SystemErrorCode

pattern ERROR_RESOURCE_NOT_AVAILABLE    = 5006 :: SystemErrorCode

pattern ERROR_RESOURCE_NOT_FOUND        = 5007 :: SystemErrorCode

pattern ERROR_SHUTDOWN_CLUSTER          = 5008 :: SystemErrorCode

pattern ERROR_CANT_EVICT_ACTIVE_NODE    = 5009 :: SystemErrorCode

pattern ERROR_OBJECT_ALREADY_EXISTS     = 5010 :: SystemErrorCode

pattern ERROR_OBJECT_IN_LIST            = 5011 :: SystemErrorCode

pattern ERROR_GROUP_NOT_AVAILABLE       = 5012 :: SystemErrorCode

pattern ERROR_GROUP_NOT_FOUND           = 5013 :: SystemErrorCode

pattern ERROR_GROUP_NOT_ONLINE          = 5014 :: SystemErrorCode

pattern ERROR_HOST_NODE_NOT_RESOURCE_OWNER= 5015 :: SystemErrorCode

pattern ERROR_HOST_NODE_NOT_GROUP_OWNER = 5016 :: SystemErrorCode

pattern ERROR_RESMON_CREATE_FAILED      = 5017 :: SystemErrorCode

pattern ERROR_RESMON_ONLINE_FAILED      = 5018 :: SystemErrorCode

pattern ERROR_RESOURCE_ONLINE           = 5019 :: SystemErrorCode

pattern ERROR_QUORUM_RESOURCE           = 5020 :: SystemErrorCode

pattern ERROR_NOT_QUORUM_CAPABLE        = 5021 :: SystemErrorCode

pattern ERROR_CLUSTER_SHUTTING_DOWN     = 5022 :: SystemErrorCode

pattern ERROR_INVALID_STATE             = 5023 :: SystemErrorCode

pattern ERROR_RESOURCE_PROPERTIES_STORED= 5024 :: SystemErrorCode

pattern ERROR_NOT_QUORUM_CLASS          = 5025 :: SystemErrorCode

pattern ERROR_CORE_RESOURCE             = 5026 :: SystemErrorCode

pattern ERROR_QUORUM_RESOURCE_ONLINE_FAILED= 5027 :: SystemErrorCode

pattern ERROR_QUORUMLOG_OPEN_FAILED     = 5028 :: SystemErrorCode

pattern ERROR_CLUSTERLOG_CORRUPT        = 5029 :: SystemErrorCode

pattern ERROR_CLUSTERLOG_RECORD_EXCEEDS_MAXSIZE= 5030 :: SystemErrorCode

pattern ERROR_CLUSTERLOG_EXCEEDS_MAXSIZE= 5031 :: SystemErrorCode

pattern ERROR_CLUSTERLOG_CHKPOINT_NOT_FOUND= 5032 :: SystemErrorCode

pattern ERROR_CLUSTERLOG_NOT_ENOUGH_SPACE= 5033 :: SystemErrorCode

pattern ERROR_QUORUM_OWNER_ALIVE        = 5034 :: SystemErrorCode

pattern ERROR_NETWORK_NOT_AVAILABLE     = 5035 :: SystemErrorCode

pattern ERROR_NODE_NOT_AVAILABLE        = 5036 :: SystemErrorCode

pattern ERROR_ALL_NODES_NOT_AVAILABLE   = 5037 :: SystemErrorCode

pattern ERROR_RESOURCE_FAILED           = 5038 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_NODE      = 5039 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_EXISTS       = 5040 :: SystemErrorCode

pattern ERROR_CLUSTER_JOIN_IN_PROGRESS  = 5041 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_NOT_FOUND    = 5042 :: SystemErrorCode

pattern ERROR_CLUSTER_LOCAL_NODE_NOT_FOUND= 5043 :: SystemErrorCode

pattern ERROR_CLUSTER_NETWORK_EXISTS    = 5044 :: SystemErrorCode

pattern ERROR_CLUSTER_NETWORK_NOT_FOUND = 5045 :: SystemErrorCode

pattern ERROR_CLUSTER_NETINTERFACE_EXISTS= 5046 :: SystemErrorCode

pattern ERROR_CLUSTER_NETINTERFACE_NOT_FOUND= 5047 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_REQUEST   = 5048 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_NETWORK_PROVIDER= 5049 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_DOWN         = 5050 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_UNREACHABLE  = 5051 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_NOT_MEMBER   = 5052 :: SystemErrorCode

pattern ERROR_CLUSTER_JOIN_NOT_IN_PROGRESS= 5053 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_NETWORK   = 5054 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_UP           = 5056 :: SystemErrorCode

pattern ERROR_CLUSTER_IPADDR_IN_USE     = 5057 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_NOT_PAUSED   = 5058 :: SystemErrorCode

pattern ERROR_CLUSTER_NO_SECURITY_CONTEXT= 5059 :: SystemErrorCode

pattern ERROR_CLUSTER_NETWORK_NOT_INTERNAL= 5060 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_ALREADY_UP   = 5061 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_ALREADY_DOWN = 5062 :: SystemErrorCode

pattern ERROR_CLUSTER_NETWORK_ALREADY_ONLINE= 5063 :: SystemErrorCode

pattern ERROR_CLUSTER_NETWORK_ALREADY_OFFLINE= 5064 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_ALREADY_MEMBER= 5065 :: SystemErrorCode

pattern ERROR_CLUSTER_LAST_INTERNAL_NETWORK= 5066 :: SystemErrorCode

pattern ERROR_CLUSTER_NETWORK_HAS_DEPENDENTS= 5067 :: SystemErrorCode

pattern ERROR_INVALID_OPERATION_ON_QUORUM= 5068 :: SystemErrorCode

pattern ERROR_DEPENDENCY_NOT_ALLOWED    = 5069 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_PAUSED       = 5070 :: SystemErrorCode

pattern ERROR_NODE_CANT_HOST_RESOURCE   = 5071 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_NOT_READY    = 5072 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_SHUTTING_DOWN= 5073 :: SystemErrorCode

pattern ERROR_CLUSTER_JOIN_ABORTED      = 5074 :: SystemErrorCode

pattern ERROR_CLUSTER_INCOMPATIBLE_VERSIONS= 5075 :: SystemErrorCode

pattern ERROR_CLUSTER_MAXNUM_OF_RESOURCES_EXCEEDED= 5076 :: SystemErrorCode

pattern ERROR_CLUSTER_SYSTEM_CONFIG_CHANGED= 5077 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_TYPE_NOT_FOUND= 5078 :: SystemErrorCode

pattern ERROR_CLUSTER_RESTYPE_NOT_SUPPORTED= 5079 :: SystemErrorCode

pattern ERROR_CLUSTER_RESNAME_NOT_FOUND = 5080 :: SystemErrorCode

pattern ERROR_CLUSTER_NO_RPC_PACKAGES_REGISTERED= 5081 :: SystemErrorCode

pattern ERROR_CLUSTER_OWNER_NOT_IN_PREFLIST= 5082 :: SystemErrorCode

pattern ERROR_CLUSTER_DATABASE_SEQMISMATCH= 5083 :: SystemErrorCode

pattern ERROR_RESMON_INVALID_STATE      = 5084 :: SystemErrorCode

pattern ERROR_CLUSTER_GUM_NOT_LOCKER    = 5085 :: SystemErrorCode

pattern ERROR_QUORUM_DISK_NOT_FOUND     = 5086 :: SystemErrorCode

pattern ERROR_DATABASE_BACKUP_CORRUPT   = 5087 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_ALREADY_HAS_DFS_ROOT= 5088 :: SystemErrorCode

pattern ERROR_RESOURCE_PROPERTY_UNCHANGEABLE= 5089 :: SystemErrorCode

pattern ERROR_NO_ADMIN_ACCESS_POINT     = 5090 :: SystemErrorCode

pattern ERROR_CLUSTER_MEMBERSHIP_INVALID_STATE= 5890 :: SystemErrorCode

pattern ERROR_CLUSTER_QUORUMLOG_NOT_FOUND= 5891 :: SystemErrorCode

pattern ERROR_CLUSTER_MEMBERSHIP_HALT   = 5892 :: SystemErrorCode

pattern ERROR_CLUSTER_INSTANCE_ID_MISMATCH= 5893 :: SystemErrorCode

pattern ERROR_CLUSTER_NETWORK_NOT_FOUND_FOR_IP= 5894 :: SystemErrorCode

pattern ERROR_CLUSTER_PROPERTY_DATA_TYPE_MISMATCH= 5895 :: SystemErrorCode

pattern ERROR_CLUSTER_EVICT_WITHOUT_CLEANUP= 5896 :: SystemErrorCode

pattern ERROR_CLUSTER_PARAMETER_MISMATCH= 5897 :: SystemErrorCode

pattern ERROR_NODE_CANNOT_BE_CLUSTERED  = 5898 :: SystemErrorCode

pattern ERROR_CLUSTER_WRONG_OS_VERSION  = 5899 :: SystemErrorCode

pattern ERROR_CLUSTER_CANT_CREATE_DUP_CLUSTER_NAME= 5900 :: SystemErrorCode

pattern ERROR_CLUSCFG_ALREADY_COMMITTED = 5901 :: SystemErrorCode

pattern ERROR_CLUSCFG_ROLLBACK_FAILED   = 5902 :: SystemErrorCode

pattern ERROR_CLUSCFG_SYSTEM_DISK_DRIVE_LETTER_CONFLICT= 5903 :: SystemErrorCode

pattern ERROR_CLUSTER_OLD_VERSION       = 5904 :: SystemErrorCode

pattern ERROR_CLUSTER_MISMATCHED_COMPUTER_ACCT_NAME= 5905 :: SystemErrorCode

pattern ERROR_CLUSTER_NO_NET_ADAPTERS   = 5906 :: SystemErrorCode

pattern ERROR_CLUSTER_POISONED          = 5907 :: SystemErrorCode

pattern ERROR_CLUSTER_GROUP_MOVING      = 5908 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_TYPE_BUSY= 5909 :: SystemErrorCode

pattern ERROR_RESOURCE_CALL_TIMED_OUT   = 5910 :: SystemErrorCode

pattern ERROR_INVALID_CLUSTER_IPV6_ADDRESS= 5911 :: SystemErrorCode

pattern ERROR_CLUSTER_INTERNAL_INVALID_FUNCTION= 5912 :: SystemErrorCode

pattern ERROR_CLUSTER_PARAMETER_OUT_OF_BOUNDS= 5913 :: SystemErrorCode

pattern ERROR_CLUSTER_PARTIAL_SEND      = 5914 :: SystemErrorCode

pattern ERROR_CLUSTER_REGISTRY_INVALID_FUNCTION= 5915 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_STRING_TERMINATION= 5916 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_STRING_FORMAT= 5917 :: SystemErrorCode

pattern ERROR_CLUSTER_DATABASE_TRANSACTION_IN_PROGRESS= 5918 :: SystemErrorCode

pattern ERROR_CLUSTER_DATABASE_TRANSACTION_NOT_IN_PROGRESS= 5919 :: SystemErrorCode

pattern ERROR_CLUSTER_NULL_DATA         = 5920 :: SystemErrorCode

pattern ERROR_CLUSTER_PARTIAL_READ      = 5921 :: SystemErrorCode

pattern ERROR_CLUSTER_PARTIAL_WRITE     = 5922 :: SystemErrorCode

pattern ERROR_CLUSTER_CANT_DESERIALIZE_DATA= 5923 :: SystemErrorCode

pattern ERROR_DEPENDENT_RESOURCE_PROPERTY_CONFLICT= 5924 :: SystemErrorCode

pattern ERROR_CLUSTER_NO_QUORUM         = 5925 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_IPV6_NETWORK= 5926 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_IPV6_TUNNEL_NETWORK= 5927 :: SystemErrorCode

pattern ERROR_QUORUM_NOT_ALLOWED_IN_THIS_GROUP= 5928 :: SystemErrorCode

pattern ERROR_DEPENDENCY_TREE_TOO_COMPLEX= 5929 :: SystemErrorCode

pattern ERROR_EXCEPTION_IN_RESOURCE_CALL= 5930 :: SystemErrorCode

pattern ERROR_CLUSTER_RHS_FAILED_INITIALIZATION= 5931 :: SystemErrorCode

pattern ERROR_CLUSTER_NOT_INSTALLED     = 5932 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCES_MUST_BE_ONLINE_ON_THE_SAME_NODE= 5933 :: SystemErrorCode

pattern ERROR_CLUSTER_MAX_NODES_IN_CLUSTER= 5934 :: SystemErrorCode

pattern ERROR_CLUSTER_TOO_MANY_NODES    = 5935 :: SystemErrorCode

pattern ERROR_CLUSTER_OBJECT_ALREADY_USED= 5936 :: SystemErrorCode

pattern ERROR_NONCORE_GROUPS_FOUND      = 5937 :: SystemErrorCode

pattern ERROR_FILE_SHARE_RESOURCE_CONFLICT= 5938 :: SystemErrorCode

pattern ERROR_CLUSTER_EVICT_INVALID_REQUEST= 5939 :: SystemErrorCode

pattern ERROR_CLUSTER_SINGLETON_RESOURCE= 5940 :: SystemErrorCode

pattern ERROR_CLUSTER_GROUP_SINGLETON_RESOURCE= 5941 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_PROVIDER_FAILED= 5942 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_CONFIGURATION_ERROR= 5943 :: SystemErrorCode

pattern ERROR_CLUSTER_GROUP_BUSY        = 5944 :: SystemErrorCode

pattern ERROR_CLUSTER_NOT_SHARED_VOLUME = 5945 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_SECURITY_DESCRIPTOR= 5946 :: SystemErrorCode

pattern ERROR_CLUSTER_SHARED_VOLUMES_IN_USE= 5947 :: SystemErrorCode

pattern ERROR_CLUSTER_USE_SHARED_VOLUMES_API= 5948 :: SystemErrorCode

pattern ERROR_CLUSTER_BACKUP_IN_PROGRESS= 5949 :: SystemErrorCode

pattern ERROR_NON_CSV_PATH              = 5950 :: SystemErrorCode

pattern ERROR_CSV_VOLUME_NOT_LOCAL      = 5951 :: SystemErrorCode

pattern ERROR_CLUSTER_WATCHDOG_TERMINATING= 5952 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_VETOED_MOVE_INCOMPATIBLE_NODES= 5953 :: SystemErrorCode

pattern ERROR_CLUSTER_INVALID_NODE_WEIGHT= 5954 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_VETOED_CALL= 5955 :: SystemErrorCode

pattern ERROR_RESMON_SYSTEM_RESOURCES_LACKING= 5956 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_VETOED_MOVE_NOT_ENOUGH_RESOURCES_ON_DESTINATION= 5957 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_VETOED_MOVE_NOT_ENOUGH_RESOURCES_ON_SOURCE= 5958 :: SystemErrorCode

pattern ERROR_CLUSTER_GROUP_QUEUED      = 5959 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_LOCKED_STATUS= 5960 :: SystemErrorCode

pattern ERROR_CLUSTER_SHARED_VOLUME_FAILOVER_NOT_ALLOWED= 5961 :: SystemErrorCode

pattern ERROR_CLUSTER_NODE_DRAIN_IN_PROGRESS= 5962 :: SystemErrorCode

pattern ERROR_CLUSTER_DISK_NOT_CONNECTED= 5963 :: SystemErrorCode

pattern ERROR_DISK_NOT_CSV_CAPABLE      = 5964 :: SystemErrorCode

pattern ERROR_RESOURCE_NOT_IN_AVAILABLE_STORAGE= 5965 :: SystemErrorCode

pattern ERROR_CLUSTER_SHARED_VOLUME_REDIRECTED= 5966 :: SystemErrorCode

pattern ERROR_CLUSTER_SHARED_VOLUME_NOT_REDIRECTED= 5967 :: SystemErrorCode

pattern ERROR_CLUSTER_CANNOT_RETURN_PROPERTIES= 5968 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_CONTAINS_UNSUPPORTED_DIFF_AREA_FOR_SHARED_VOLUMES= 5969 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_IS_IN_MAINTENANCE_MODE= 5970 :: SystemErrorCode

pattern ERROR_CLUSTER_AFFINITY_CONFLICT = 5971 :: SystemErrorCode

pattern ERROR_CLUSTER_RESOURCE_IS_REPLICA_VIRTUAL_MACHINE= 5972 :: SystemErrorCode

pattern ERROR_ENCRYPTION_FAILED         = 6000 :: SystemErrorCode

pattern ERROR_DECRYPTION_FAILED         = 6001 :: SystemErrorCode

pattern ERROR_FILE_ENCRYPTED            = 6002 :: SystemErrorCode

pattern ERROR_NO_RECOVERY_POLICY        = 6003 :: SystemErrorCode

pattern ERROR_NO_EFS                    = 6004 :: SystemErrorCode

pattern ERROR_WRONG_EFS                 = 6005 :: SystemErrorCode

pattern ERROR_NO_USER_KEYS              = 6006 :: SystemErrorCode

pattern ERROR_FILE_NOT_ENCRYPTED        = 6007 :: SystemErrorCode

pattern ERROR_NOT_EXPORT_FORMAT         = 6008 :: SystemErrorCode

pattern ERROR_FILE_READ_ONLY            = 6009 :: SystemErrorCode

pattern ERROR_DIR_EFS_DISALLOWED        = 6010 :: SystemErrorCode

pattern ERROR_EFS_SERVER_NOT_TRUSTED    = 6011 :: SystemErrorCode

pattern ERROR_BAD_RECOVERY_POLICY       = 6012 :: SystemErrorCode

pattern ERROR_EFS_ALG_BLOB_TOO_BIG      = 6013 :: SystemErrorCode

pattern ERROR_VOLUME_NOT_SUPPORT_EFS    = 6014 :: SystemErrorCode

pattern ERROR_EFS_DISABLED              = 6015 :: SystemErrorCode

pattern ERROR_EFS_VERSION_NOT_SUPPORT   = 6016 :: SystemErrorCode

pattern ERROR_CS_ENCRYPTION_INVALID_SERVER_RESPONSE= 6017 :: SystemErrorCode

pattern ERROR_CS_ENCRYPTION_UNSUPPORTED_SERVER= 6018 :: SystemErrorCode

pattern ERROR_CS_ENCRYPTION_EXISTING_ENCRYPTED_FILE= 6019 :: SystemErrorCode

pattern ERROR_CS_ENCRYPTION_NEW_ENCRYPTED_FILE= 6020 :: SystemErrorCode

pattern ERROR_CS_ENCRYPTION_FILE_NOT_CSE= 6021 :: SystemErrorCode

pattern ERROR_ENCRYPTION_POLICY_DENIES_OPERATION= 6022 :: SystemErrorCode

pattern ERROR_NO_BROWSER_SERVERS_FOUND  = 6118 :: SystemErrorCode

pattern ERROR_LOG_SECTOR_INVALID        = 6600 :: SystemErrorCode

pattern ERROR_LOG_SECTOR_PARITY_INVALID = 6601 :: SystemErrorCode

pattern ERROR_LOG_SECTOR_REMAPPED       = 6602 :: SystemErrorCode

pattern ERROR_LOG_BLOCK_INCOMPLETE      = 6603 :: SystemErrorCode

pattern ERROR_LOG_INVALID_RANGE         = 6604 :: SystemErrorCode

pattern ERROR_LOG_BLOCKS_EXHAUSTED      = 6605 :: SystemErrorCode

pattern ERROR_LOG_READ_CONTEXT_INVALID  = 6606 :: SystemErrorCode

pattern ERROR_LOG_RESTART_INVALID       = 6607 :: SystemErrorCode

pattern ERROR_LOG_BLOCK_VERSION         = 6608 :: SystemErrorCode

pattern ERROR_LOG_BLOCK_INVALID         = 6609 :: SystemErrorCode

pattern ERROR_LOG_READ_MODE_INVALID     = 6610 :: SystemErrorCode

pattern ERROR_LOG_NO_RESTART            = 6611 :: SystemErrorCode

pattern ERROR_LOG_METADATA_CORRUPT      = 6612 :: SystemErrorCode

pattern ERROR_LOG_METADATA_INVALID      = 6613 :: SystemErrorCode

pattern ERROR_LOG_METADATA_INCONSISTENT = 6614 :: SystemErrorCode

pattern ERROR_LOG_RESERVATION_INVALID   = 6615 :: SystemErrorCode

pattern ERROR_LOG_CANT_DELETE           = 6616 :: SystemErrorCode

pattern ERROR_LOG_CONTAINER_LIMIT_EXCEEDED= 6617 :: SystemErrorCode

pattern ERROR_LOG_START_OF_LOG          = 6618 :: SystemErrorCode

pattern ERROR_LOG_POLICY_ALREADY_INSTALLED= 6619 :: SystemErrorCode

pattern ERROR_LOG_POLICY_NOT_INSTALLED  = 6620 :: SystemErrorCode

pattern ERROR_LOG_POLICY_INVALID        = 6621 :: SystemErrorCode

pattern ERROR_LOG_POLICY_CONFLICT       = 6622 :: SystemErrorCode

pattern ERROR_LOG_PINNED_ARCHIVE_TAIL   = 6623 :: SystemErrorCode

pattern ERROR_LOG_RECORD_NONEXISTENT    = 6624 :: SystemErrorCode

pattern ERROR_LOG_RECORDS_RESERVED_INVALID= 6625 :: SystemErrorCode

pattern ERROR_LOG_SPACE_RESERVED_INVALID= 6626 :: SystemErrorCode

pattern ERROR_LOG_TAIL_INVALID          = 6627 :: SystemErrorCode

pattern ERROR_LOG_FULL                  = 6628 :: SystemErrorCode

pattern ERROR_COULD_NOT_RESIZE_LOG      = 6629 :: SystemErrorCode

pattern ERROR_LOG_MULTIPLEXED           = 6630 :: SystemErrorCode

pattern ERROR_LOG_DEDICATED             = 6631 :: SystemErrorCode

pattern ERROR_LOG_ARCHIVE_NOT_IN_PROGRESS= 6632 :: SystemErrorCode

pattern ERROR_LOG_ARCHIVE_IN_PROGRESS   = 6633 :: SystemErrorCode

pattern ERROR_LOG_EPHEMERAL             = 6634 :: SystemErrorCode

pattern ERROR_LOG_NOT_ENOUGH_CONTAINERS = 6635 :: SystemErrorCode

pattern ERROR_LOG_CLIENT_ALREADY_REGISTERED= 6636 :: SystemErrorCode

pattern ERROR_LOG_CLIENT_NOT_REGISTERED = 6637 :: SystemErrorCode

pattern ERROR_LOG_FULL_HANDLER_IN_PROGRESS= 6638 :: SystemErrorCode

pattern ERROR_LOG_CONTAINER_READ_FAILED = 6639 :: SystemErrorCode

pattern ERROR_LOG_CONTAINER_WRITE_FAILED= 6640 :: SystemErrorCode

pattern ERROR_LOG_CONTAINER_OPEN_FAILED = 6641 :: SystemErrorCode

pattern ERROR_LOG_CONTAINER_STATE_INVALID= 6642 :: SystemErrorCode

pattern ERROR_LOG_STATE_INVALID         = 6643 :: SystemErrorCode

pattern ERROR_LOG_PINNED                = 6644 :: SystemErrorCode

pattern ERROR_LOG_METADATA_FLUSH_FAILED = 6645 :: SystemErrorCode

pattern ERROR_LOG_INCONSISTENT_SECURITY = 6646 :: SystemErrorCode

pattern ERROR_LOG_APPENDED_FLUSH_FAILED = 6647 :: SystemErrorCode

pattern ERROR_LOG_PINNED_RESERVATION    = 6648 :: SystemErrorCode

pattern ERROR_INVALID_TRANSACTION       = 6700 :: SystemErrorCode

pattern ERROR_TRANSACTION_NOT_ACTIVE    = 6701 :: SystemErrorCode

pattern ERROR_TRANSACTION_REQUEST_NOT_VALID= 6702 :: SystemErrorCode

pattern ERROR_TRANSACTION_NOT_REQUESTED = 6703 :: SystemErrorCode

pattern ERROR_TRANSACTION_ALREADY_ABORTED= 6704 :: SystemErrorCode

pattern ERROR_TRANSACTION_ALREADY_COMMITTED= 6705 :: SystemErrorCode

pattern ERROR_TM_INITIALIZATION_FAILED  = 6706 :: SystemErrorCode

pattern ERROR_RESOURCEMANAGER_READ_ONLY = 6707 :: SystemErrorCode

pattern ERROR_TRANSACTION_NOT_JOINED    = 6708 :: SystemErrorCode

pattern ERROR_TRANSACTION_SUPERIOR_EXISTS= 6709 :: SystemErrorCode

pattern ERROR_CRM_PROTOCOL_ALREADY_EXISTS= 6710 :: SystemErrorCode

pattern ERROR_TRANSACTION_PROPAGATION_FAILED= 6711 :: SystemErrorCode

pattern ERROR_CRM_PROTOCOL_NOT_FOUND    = 6712 :: SystemErrorCode

pattern ERROR_TRANSACTION_INVALID_MARSHALL_BUFFER= 6713 :: SystemErrorCode

pattern ERROR_CURRENT_TRANSACTION_NOT_VALID= 6714 :: SystemErrorCode

pattern ERROR_TRANSACTION_NOT_FOUND     = 6715 :: SystemErrorCode

pattern ERROR_RESOURCEMANAGER_NOT_FOUND = 6716 :: SystemErrorCode

pattern ERROR_ENLISTMENT_NOT_FOUND      = 6717 :: SystemErrorCode

pattern ERROR_TRANSACTIONMANAGER_NOT_FOUND= 6718 :: SystemErrorCode

pattern ERROR_TRANSACTIONMANAGER_NOT_ONLINE= 6719 :: SystemErrorCode

pattern ERROR_TRANSACTIONMANAGER_RECOVERY_NAME_COLLISION= 6720 :: SystemErrorCode

pattern ERROR_TRANSACTION_NOT_ROOT      = 6721 :: SystemErrorCode

pattern ERROR_TRANSACTION_OBJECT_EXPIRED= 6722 :: SystemErrorCode

pattern ERROR_TRANSACTION_RESPONSE_NOT_ENLISTED= 6723 :: SystemErrorCode

pattern ERROR_TRANSACTION_RECORD_TOO_LONG= 6724 :: SystemErrorCode

pattern ERROR_IMPLICIT_TRANSACTION_NOT_SUPPORTED= 6725 :: SystemErrorCode

pattern ERROR_TRANSACTION_INTEGRITY_VIOLATED= 6726 :: SystemErrorCode

pattern ERROR_TRANSACTIONMANAGER_IDENTITY_MISMATCH= 6727 :: SystemErrorCode

pattern ERROR_RM_CANNOT_BE_FROZEN_FOR_SNAPSHOT= 6728 :: SystemErrorCode

pattern ERROR_TRANSACTION_MUST_WRITETHROUGH= 6729 :: SystemErrorCode

pattern ERROR_TRANSACTION_NO_SUPERIOR   = 6730 :: SystemErrorCode

pattern ERROR_HEURISTIC_DAMAGE_POSSIBLE = 6731 :: SystemErrorCode

pattern ERROR_TRANSACTIONAL_CONFLICT    = 6800 :: SystemErrorCode

pattern ERROR_RM_NOT_ACTIVE             = 6801 :: SystemErrorCode

pattern ERROR_RM_METADATA_CORRUPT       = 6802 :: SystemErrorCode

pattern ERROR_DIRECTORY_NOT_RM          = 6803 :: SystemErrorCode

pattern ERROR_TRANSACTIONS_UNSUPPORTED_REMOTE= 6805 :: SystemErrorCode

pattern ERROR_LOG_RESIZE_INVALID_SIZE   = 6806 :: SystemErrorCode

pattern ERROR_OBJECT_NO_LONGER_EXISTS   = 6807 :: SystemErrorCode

pattern ERROR_STREAM_MINIVERSION_NOT_FOUND= 6808 :: SystemErrorCode

pattern ERROR_STREAM_MINIVERSION_NOT_VALID= 6809 :: SystemErrorCode

pattern ERROR_MINIVERSION_INACCESSIBLE_FROM_SPECIFIED_TRANSACTION= 6810 :: SystemErrorCode

pattern ERROR_CANT_OPEN_MINIVERSION_WITH_MODIFY_INTENT= 6811 :: SystemErrorCode

pattern ERROR_CANT_CREATE_MORE_STREAM_MINIVERSIONS= 6812 :: SystemErrorCode

pattern ERROR_REMOTE_FILE_VERSION_MISMATCH= 6814 :: SystemErrorCode

pattern ERROR_HANDLE_NO_LONGER_VALID    = 6815 :: SystemErrorCode

pattern ERROR_NO_TXF_METADATA           = 6816 :: SystemErrorCode

pattern ERROR_LOG_CORRUPTION_DETECTED   = 6817 :: SystemErrorCode

pattern ERROR_CANT_RECOVER_WITH_HANDLE_OPEN= 6818 :: SystemErrorCode

pattern ERROR_RM_DISCONNECTED           = 6819 :: SystemErrorCode

pattern ERROR_ENLISTMENT_NOT_SUPERIOR   = 6820 :: SystemErrorCode

pattern ERROR_RECOVERY_NOT_NEEDED       = 6821 :: SystemErrorCode

pattern ERROR_RM_ALREADY_STARTED        = 6822 :: SystemErrorCode

pattern ERROR_FILE_IDENTITY_NOT_PERSISTENT= 6823 :: SystemErrorCode

pattern ERROR_CANT_BREAK_TRANSACTIONAL_DEPENDENCY= 6824 :: SystemErrorCode

pattern ERROR_CANT_CROSS_RM_BOUNDARY    = 6825 :: SystemErrorCode

pattern ERROR_TXF_DIR_NOT_EMPTY         = 6826 :: SystemErrorCode

pattern ERROR_INDOUBT_TRANSACTIONS_EXIST= 6827 :: SystemErrorCode

pattern ERROR_TM_VOLATILE               = 6828 :: SystemErrorCode

pattern ERROR_ROLLBACK_TIMER_EXPIRED    = 6829 :: SystemErrorCode

pattern ERROR_TXF_ATTRIBUTE_CORRUPT     = 6830 :: SystemErrorCode

pattern ERROR_EFS_NOT_ALLOWED_IN_TRANSACTION= 6831 :: SystemErrorCode

pattern ERROR_TRANSACTIONAL_OPEN_NOT_ALLOWED= 6832 :: SystemErrorCode

pattern ERROR_LOG_GROWTH_FAILED         = 6833 :: SystemErrorCode

pattern ERROR_TRANSACTED_MAPPING_UNSUPPORTED_REMOTE= 6834 :: SystemErrorCode

pattern ERROR_TXF_METADATA_ALREADY_PRESENT= 6835 :: SystemErrorCode

pattern ERROR_TRANSACTION_SCOPE_CALLBACKS_NOT_SET= 6836 :: SystemErrorCode

pattern ERROR_TRANSACTION_REQUIRED_PROMOTION= 6837 :: SystemErrorCode

pattern ERROR_CANNOT_EXECUTE_FILE_IN_TRANSACTION= 6838 :: SystemErrorCode

pattern ERROR_TRANSACTIONS_NOT_FROZEN   = 6839 :: SystemErrorCode

pattern ERROR_TRANSACTION_FREEZE_IN_PROGRESS= 6840 :: SystemErrorCode

pattern ERROR_NOT_SNAPSHOT_VOLUME       = 6841 :: SystemErrorCode

pattern ERROR_NO_SAVEPOINT_WITH_OPEN_FILES= 6842 :: SystemErrorCode

pattern ERROR_DATA_LOST_REPAIR          = 6843 :: SystemErrorCode

pattern ERROR_SPARSE_NOT_ALLOWED_IN_TRANSACTION= 6844 :: SystemErrorCode

pattern ERROR_TM_IDENTITY_MISMATCH      = 6845 :: SystemErrorCode

pattern ERROR_FLOATED_SECTION           = 6846 :: SystemErrorCode

pattern ERROR_CANNOT_ACCEPT_TRANSACTED_WORK= 6847 :: SystemErrorCode

pattern ERROR_CANNOT_ABORT_TRANSACTIONS = 6848 :: SystemErrorCode

pattern ERROR_BAD_CLUSTERS              = 6849 :: SystemErrorCode

pattern ERROR_COMPRESSION_NOT_ALLOWED_IN_TRANSACTION= 6850 :: SystemErrorCode

pattern ERROR_VOLUME_DIRTY              = 6851 :: SystemErrorCode

pattern ERROR_NO_LINK_TRACKING_IN_TRANSACTION= 6852 :: SystemErrorCode

pattern ERROR_OPERATION_NOT_SUPPORTED_IN_TRANSACTION= 6853 :: SystemErrorCode

pattern ERROR_EXPIRED_HANDLE            = 6854 :: SystemErrorCode

pattern ERROR_TRANSACTION_NOT_ENLISTED  = 6855 :: SystemErrorCode

pattern ERROR_CTX_WINSTATION_NAME_INVALID= 7001 :: SystemErrorCode

pattern ERROR_CTX_INVALID_PD            = 7002 :: SystemErrorCode

pattern ERROR_CTX_PD_NOT_FOUND          = 7003 :: SystemErrorCode

pattern ERROR_CTX_WD_NOT_FOUND          = 7004 :: SystemErrorCode

pattern ERROR_CTX_CANNOT_MAKE_EVENTLOG_ENTRY= 7005 :: SystemErrorCode

pattern ERROR_CTX_SERVICE_NAME_COLLISION= 7006 :: SystemErrorCode

pattern ERROR_CTX_CLOSE_PENDING         = 7007 :: SystemErrorCode

pattern ERROR_CTX_NO_OUTBUF             = 7008 :: SystemErrorCode

pattern ERROR_CTX_MODEM_INF_NOT_FOUND   = 7009 :: SystemErrorCode

pattern ERROR_CTX_INVALID_MODEMNAME     = 7010 :: SystemErrorCode

pattern ERROR_CTX_MODEM_RESPONSE_ERROR  = 7011 :: SystemErrorCode

pattern ERROR_CTX_MODEM_RESPONSE_TIMEOUT= 7012 :: SystemErrorCode

pattern ERROR_CTX_MODEM_RESPONSE_NO_CARRIER= 7013 :: SystemErrorCode

pattern ERROR_CTX_MODEM_RESPONSE_NO_DIALTONE= 7014 :: SystemErrorCode

pattern ERROR_CTX_MODEM_RESPONSE_BUSY   = 7015 :: SystemErrorCode

pattern ERROR_CTX_MODEM_RESPONSE_VOICE  = 7016 :: SystemErrorCode

pattern ERROR_CTX_TD_ERROR              = 7017 :: SystemErrorCode

pattern ERROR_CTX_WINSTATION_NOT_FOUND  = 7022 :: SystemErrorCode

pattern ERROR_CTX_WINSTATION_ALREADY_EXISTS= 7023 :: SystemErrorCode

pattern ERROR_CTX_WINSTATION_BUSY       = 7024 :: SystemErrorCode

pattern ERROR_CTX_BAD_VIDEO_MODE        = 7025 :: SystemErrorCode

pattern ERROR_CTX_GRAPHICS_INVALID      = 7035 :: SystemErrorCode

pattern ERROR_CTX_LOGON_DISABLED        = 7037 :: SystemErrorCode

pattern ERROR_CTX_NOT_CONSOLE           = 7038 :: SystemErrorCode

pattern ERROR_CTX_CLIENT_QUERY_TIMEOUT  = 7040 :: SystemErrorCode

pattern ERROR_CTX_CONSOLE_DISCONNECT    = 7041 :: SystemErrorCode

pattern ERROR_CTX_CONSOLE_CONNECT       = 7042 :: SystemErrorCode

pattern ERROR_CTX_SHADOW_DENIED         = 7044 :: SystemErrorCode

pattern ERROR_CTX_WINSTATION_ACCESS_DENIED= 7045 :: SystemErrorCode

pattern ERROR_CTX_INVALID_WD            = 7049 :: SystemErrorCode

pattern ERROR_CTX_SHADOW_INVALID        = 7050 :: SystemErrorCode

pattern ERROR_CTX_SHADOW_DISABLED       = 7051 :: SystemErrorCode

pattern ERROR_CTX_CLIENT_LICENSE_IN_USE = 7052 :: SystemErrorCode

pattern ERROR_CTX_CLIENT_LICENSE_NOT_SET= 7053 :: SystemErrorCode

pattern ERROR_CTX_LICENSE_NOT_AVAILABLE = 7054 :: SystemErrorCode

pattern ERROR_CTX_LICENSE_CLIENT_INVALID= 7055 :: SystemErrorCode

pattern ERROR_CTX_LICENSE_EXPIRED       = 7056 :: SystemErrorCode

pattern ERROR_CTX_SHADOW_NOT_RUNNING    = 7057 :: SystemErrorCode

pattern ERROR_CTX_SHADOW_ENDED_BY_MODE_CHANGE= 7058 :: SystemErrorCode

pattern ERROR_ACTIVATION_COUNT_EXCEEDED = 7059 :: SystemErrorCode

pattern ERROR_CTX_WINSTATIONS_DISABLED  = 7060 :: SystemErrorCode

pattern ERROR_CTX_ENCRYPTION_LEVEL_REQUIRED= 7061 :: SystemErrorCode

pattern ERROR_CTX_SESSION_IN_USE        = 7062 :: SystemErrorCode

pattern ERROR_CTX_NO_FORCE_LOGOFF       = 7063 :: SystemErrorCode

pattern ERROR_CTX_ACCOUNT_RESTRICTION   = 7064 :: SystemErrorCode

pattern ERROR_RDP_PROTOCOL_ERROR        = 7065 :: SystemErrorCode

pattern ERROR_CTX_CDM_CONNECT           = 7066 :: SystemErrorCode

pattern ERROR_CTX_CDM_DISCONNECT        = 7067 :: SystemErrorCode

pattern ERROR_CTX_SECURITY_LAYER_ERROR  = 7068 :: SystemErrorCode

pattern ERROR_TS_INCOMPATIBLE_SESSIONS  = 7069 :: SystemErrorCode

pattern ERROR_TS_VIDEO_SUBSYSTEM_ERROR  = 7070 :: SystemErrorCode

pattern ERROR_DS_NOT_INSTALLED          = 8200 :: SystemErrorCode

pattern ERROR_DS_MEMBERSHIP_EVALUATED_LOCALLY= 8201 :: SystemErrorCode

pattern ERROR_DS_NO_ATTRIBUTE_OR_VALUE  = 8202 :: SystemErrorCode

pattern ERROR_DS_INVALID_ATTRIBUTE_SYNTAX= 8203 :: SystemErrorCode

pattern ERROR_DS_ATTRIBUTE_TYPE_UNDEFINED= 8204 :: SystemErrorCode

pattern ERROR_DS_ATTRIBUTE_OR_VALUE_EXISTS= 8205 :: SystemErrorCode

pattern ERROR_DS_BUSY                   = 8206 :: SystemErrorCode

pattern ERROR_DS_UNAVAILABLE            = 8207 :: SystemErrorCode

pattern ERROR_DS_NO_RIDS_ALLOCATED      = 8208 :: SystemErrorCode

pattern ERROR_DS_NO_MORE_RIDS           = 8209 :: SystemErrorCode

pattern ERROR_DS_INCORRECT_ROLE_OWNER   = 8210 :: SystemErrorCode

pattern ERROR_DS_RIDMGR_INIT_ERROR      = 8211 :: SystemErrorCode

pattern ERROR_DS_OBJ_CLASS_VIOLATION    = 8212 :: SystemErrorCode

pattern ERROR_DS_CANT_ON_NON_LEAF       = 8213 :: SystemErrorCode

pattern ERROR_DS_CANT_ON_RDN            = 8214 :: SystemErrorCode

pattern ERROR_DS_CANT_MOD_OBJ_CLASS     = 8215 :: SystemErrorCode

pattern ERROR_DS_CROSS_DOM_MOVE_ERROR   = 8216 :: SystemErrorCode

pattern ERROR_DS_GC_NOT_AVAILABLE       = 8217 :: SystemErrorCode

pattern ERROR_SHARED_POLICY             = 8218 :: SystemErrorCode

pattern ERROR_POLICY_OBJECT_NOT_FOUND   = 8219 :: SystemErrorCode

pattern ERROR_POLICY_ONLY_IN_DS         = 8220 :: SystemErrorCode

pattern ERROR_PROMOTION_ACTIVE          = 8221 :: SystemErrorCode

pattern ERROR_NO_PROMOTION_ACTIVE       = 8222 :: SystemErrorCode

pattern ERROR_DS_OPERATIONS_ERROR       = 8224 :: SystemErrorCode

pattern ERROR_DS_PROTOCOL_ERROR         = 8225 :: SystemErrorCode

pattern ERROR_DS_TIMELIMIT_EXCEEDED     = 8226 :: SystemErrorCode

pattern ERROR_DS_SIZELIMIT_EXCEEDED     = 8227 :: SystemErrorCode

pattern ERROR_DS_ADMIN_LIMIT_EXCEEDED   = 8228 :: SystemErrorCode

pattern ERROR_DS_COMPARE_FALSE          = 8229 :: SystemErrorCode

pattern ERROR_DS_COMPARE_TRUE           = 8230 :: SystemErrorCode

pattern ERROR_DS_AUTH_METHOD_NOT_SUPPORTED= 8231 :: SystemErrorCode

pattern ERROR_DS_STRONG_AUTH_REQUIRED   = 8232 :: SystemErrorCode

pattern ERROR_DS_INAPPROPRIATE_AUTH     = 8233 :: SystemErrorCode

pattern ERROR_DS_AUTH_UNKNOWN           = 8234 :: SystemErrorCode

pattern ERROR_DS_REFERRAL               = 8235 :: SystemErrorCode

pattern ERROR_DS_UNAVAILABLE_CRIT_EXTENSION= 8236 :: SystemErrorCode

pattern ERROR_DS_CONFIDENTIALITY_REQUIRED= 8237 :: SystemErrorCode

pattern ERROR_DS_INAPPROPRIATE_MATCHING = 8238 :: SystemErrorCode

pattern ERROR_DS_CONSTRAINT_VIOLATION   = 8239 :: SystemErrorCode

pattern ERROR_DS_NO_SUCH_OBJECT         = 8240 :: SystemErrorCode

pattern ERROR_DS_ALIAS_PROBLEM          = 8241 :: SystemErrorCode

pattern ERROR_DS_INVALID_DN_SYNTAX      = 8242 :: SystemErrorCode

pattern ERROR_DS_IS_LEAF                = 8243 :: SystemErrorCode

pattern ERROR_DS_ALIAS_DEREF_PROBLEM    = 8244 :: SystemErrorCode

pattern ERROR_DS_UNWILLING_TO_PERFORM   = 8245 :: SystemErrorCode

pattern ERROR_DS_LOOP_DETECT            = 8246 :: SystemErrorCode

pattern ERROR_DS_NAMING_VIOLATION       = 8247 :: SystemErrorCode

pattern ERROR_DS_OBJECT_RESULTS_TOO_LARGE= 8248 :: SystemErrorCode

pattern ERROR_DS_AFFECTS_MULTIPLE_DSAS  = 8249 :: SystemErrorCode

pattern ERROR_DS_SERVER_DOWN            = 8250 :: SystemErrorCode

pattern ERROR_DS_LOCAL_ERROR            = 8251 :: SystemErrorCode

pattern ERROR_DS_ENCODING_ERROR         = 8252 :: SystemErrorCode

pattern ERROR_DS_DECODING_ERROR         = 8253 :: SystemErrorCode

pattern ERROR_DS_FILTER_UNKNOWN         = 8254 :: SystemErrorCode

pattern ERROR_DS_PARAM_ERROR            = 8255 :: SystemErrorCode

pattern ERROR_DS_NOT_SUPPORTED          = 8256 :: SystemErrorCode

pattern ERROR_DS_NO_RESULTS_RETURNED    = 8257 :: SystemErrorCode

pattern ERROR_DS_CONTROL_NOT_FOUND      = 8258 :: SystemErrorCode

pattern ERROR_DS_CLIENT_LOOP            = 8259 :: SystemErrorCode

pattern ERROR_DS_REFERRAL_LIMIT_EXCEEDED= 8260 :: SystemErrorCode

pattern ERROR_DS_SORT_CONTROL_MISSING   = 8261 :: SystemErrorCode

pattern ERROR_DS_OFFSET_RANGE_ERROR     = 8262 :: SystemErrorCode

pattern ERROR_DS_RIDMGR_DISABLED        = 8263 :: SystemErrorCode

pattern ERROR_DS_ROOT_MUST_BE_NC        = 8301 :: SystemErrorCode

pattern ERROR_DS_ADD_REPLICA_INHIBITED  = 8302 :: SystemErrorCode

pattern ERROR_DS_ATT_NOT_DEF_IN_SCHEMA  = 8303 :: SystemErrorCode

pattern ERROR_DS_MAX_OBJ_SIZE_EXCEEDED  = 8304 :: SystemErrorCode

pattern ERROR_DS_OBJ_STRING_NAME_EXISTS = 8305 :: SystemErrorCode

pattern ERROR_DS_NO_RDN_DEFINED_IN_SCHEMA= 8306 :: SystemErrorCode

pattern ERROR_DS_RDN_DOESNT_MATCH_SCHEMA= 8307 :: SystemErrorCode

pattern ERROR_DS_NO_REQUESTED_ATTS_FOUND= 8308 :: SystemErrorCode

pattern ERROR_DS_USER_BUFFER_TO_SMALL   = 8309 :: SystemErrorCode

pattern ERROR_DS_ATT_IS_NOT_ON_OBJ      = 8310 :: SystemErrorCode

pattern ERROR_DS_ILLEGAL_MOD_OPERATION  = 8311 :: SystemErrorCode

pattern ERROR_DS_OBJ_TOO_LARGE          = 8312 :: SystemErrorCode

pattern ERROR_DS_BAD_INSTANCE_TYPE      = 8313 :: SystemErrorCode

pattern ERROR_DS_MASTERDSA_REQUIRED     = 8314 :: SystemErrorCode

pattern ERROR_DS_OBJECT_CLASS_REQUIRED  = 8315 :: SystemErrorCode

pattern ERROR_DS_MISSING_REQUIRED_ATT   = 8316 :: SystemErrorCode

pattern ERROR_DS_ATT_NOT_DEF_FOR_CLASS  = 8317 :: SystemErrorCode

pattern ERROR_DS_ATT_ALREADY_EXISTS     = 8318 :: SystemErrorCode

pattern ERROR_DS_CANT_ADD_ATT_VALUES    = 8320 :: SystemErrorCode

pattern ERROR_DS_SINGLE_VALUE_CONSTRAINT= 8321 :: SystemErrorCode

pattern ERROR_DS_RANGE_CONSTRAINT       = 8322 :: SystemErrorCode

pattern ERROR_DS_ATT_VAL_ALREADY_EXISTS = 8323 :: SystemErrorCode

pattern ERROR_DS_CANT_REM_MISSING_ATT   = 8324 :: SystemErrorCode

pattern ERROR_DS_CANT_REM_MISSING_ATT_VAL= 8325 :: SystemErrorCode

pattern ERROR_DS_ROOT_CANT_BE_SUBREF    = 8326 :: SystemErrorCode

pattern ERROR_DS_NO_CHAINING            = 8327 :: SystemErrorCode

pattern ERROR_DS_NO_CHAINED_EVAL        = 8328 :: SystemErrorCode

pattern ERROR_DS_NO_PARENT_OBJECT       = 8329 :: SystemErrorCode

pattern ERROR_DS_PARENT_IS_AN_ALIAS     = 8330 :: SystemErrorCode

pattern ERROR_DS_CANT_MIX_MASTER_AND_REPS= 8331 :: SystemErrorCode

pattern ERROR_DS_CHILDREN_EXIST         = 8332 :: SystemErrorCode

pattern ERROR_DS_OBJ_NOT_FOUND          = 8333 :: SystemErrorCode

pattern ERROR_DS_ALIASED_OBJ_MISSING    = 8334 :: SystemErrorCode

pattern ERROR_DS_BAD_NAME_SYNTAX        = 8335 :: SystemErrorCode

pattern ERROR_DS_ALIAS_POINTS_TO_ALIAS  = 8336 :: SystemErrorCode

pattern ERROR_DS_CANT_DEREF_ALIAS       = 8337 :: SystemErrorCode

pattern ERROR_DS_OUT_OF_SCOPE           = 8338 :: SystemErrorCode

pattern ERROR_DS_OBJECT_BEING_REMOVED   = 8339 :: SystemErrorCode

pattern ERROR_DS_CANT_DELETE_DSA_OBJ    = 8340 :: SystemErrorCode

pattern ERROR_DS_GENERIC_ERROR          = 8341 :: SystemErrorCode

pattern ERROR_DS_DSA_MUST_BE_INT_MASTER = 8342 :: SystemErrorCode

pattern ERROR_DS_CLASS_NOT_DSA          = 8343 :: SystemErrorCode

pattern ERROR_DS_INSUFF_ACCESS_RIGHTS   = 8344 :: SystemErrorCode

pattern ERROR_DS_ILLEGAL_SUPERIOR       = 8345 :: SystemErrorCode

pattern ERROR_DS_ATTRIBUTE_OWNED_BY_SAM = 8346 :: SystemErrorCode

pattern ERROR_DS_NAME_TOO_MANY_PARTS    = 8347 :: SystemErrorCode

pattern ERROR_DS_NAME_TOO_LONG          = 8348 :: SystemErrorCode

pattern ERROR_DS_NAME_VALUE_TOO_LONG    = 8349 :: SystemErrorCode

pattern ERROR_DS_NAME_UNPARSEABLE       = 8350 :: SystemErrorCode

pattern ERROR_DS_NAME_TYPE_UNKNOWN      = 8351 :: SystemErrorCode

pattern ERROR_DS_NOT_AN_OBJECT          = 8352 :: SystemErrorCode

pattern ERROR_DS_SEC_DESC_TOO_SHORT     = 8353 :: SystemErrorCode

pattern ERROR_DS_SEC_DESC_INVALID       = 8354 :: SystemErrorCode

pattern ERROR_DS_NO_DELETED_NAME        = 8355 :: SystemErrorCode

pattern ERROR_DS_SUBREF_MUST_HAVE_PARENT= 8356 :: SystemErrorCode

pattern ERROR_DS_NCNAME_MUST_BE_NC      = 8357 :: SystemErrorCode

pattern ERROR_DS_CANT_ADD_SYSTEM_ONLY   = 8358 :: SystemErrorCode

pattern ERROR_DS_CLASS_MUST_BE_CONCRETE = 8359 :: SystemErrorCode

pattern ERROR_DS_INVALID_DMD            = 8360 :: SystemErrorCode

pattern ERROR_DS_OBJ_GUID_EXISTS        = 8361 :: SystemErrorCode

pattern ERROR_DS_NOT_ON_BACKLINK        = 8362 :: SystemErrorCode

pattern ERROR_DS_NO_CROSSREF_FOR_NC     = 8363 :: SystemErrorCode

pattern ERROR_DS_SHUTTING_DOWN          = 8364 :: SystemErrorCode

pattern ERROR_DS_UNKNOWN_OPERATION      = 8365 :: SystemErrorCode

pattern ERROR_DS_INVALID_ROLE_OWNER     = 8366 :: SystemErrorCode

pattern ERROR_DS_COULDNT_CONTACT_FSMO   = 8367 :: SystemErrorCode

pattern ERROR_DS_CROSS_NC_DN_RENAME     = 8368 :: SystemErrorCode

pattern ERROR_DS_CANT_MOD_SYSTEM_ONLY   = 8369 :: SystemErrorCode

pattern ERROR_DS_REPLICATOR_ONLY        = 8370 :: SystemErrorCode

pattern ERROR_DS_OBJ_CLASS_NOT_DEFINED  = 8371 :: SystemErrorCode

pattern ERROR_DS_OBJ_CLASS_NOT_SUBCLASS = 8372 :: SystemErrorCode

pattern ERROR_DS_NAME_REFERENCE_INVALID = 8373 :: SystemErrorCode

pattern ERROR_DS_CROSS_REF_EXISTS       = 8374 :: SystemErrorCode

pattern ERROR_DS_CANT_DEL_MASTER_CROSSREF= 8375 :: SystemErrorCode

pattern ERROR_DS_SUBTREE_NOTIFY_NOT_NC_HEAD= 8376 :: SystemErrorCode

pattern ERROR_DS_NOTIFY_FILTER_TOO_COMPLEX= 8377 :: SystemErrorCode

pattern ERROR_DS_DUP_RDN                = 8378 :: SystemErrorCode

pattern ERROR_DS_DUP_OID                = 8379 :: SystemErrorCode

pattern ERROR_DS_DUP_MAPI_ID            = 8380 :: SystemErrorCode

pattern ERROR_DS_DUP_SCHEMA_ID_GUID     = 8381 :: SystemErrorCode

pattern ERROR_DS_DUP_LDAP_DISPLAY_NAME  = 8382 :: SystemErrorCode

pattern ERROR_DS_SEMANTIC_ATT_TEST      = 8383 :: SystemErrorCode

pattern ERROR_DS_SYNTAX_MISMATCH        = 8384 :: SystemErrorCode

pattern ERROR_DS_EXISTS_IN_MUST_HAVE    = 8385 :: SystemErrorCode

pattern ERROR_DS_EXISTS_IN_MAY_HAVE     = 8386 :: SystemErrorCode

pattern ERROR_DS_NONEXISTENT_MAY_HAVE   = 8387 :: SystemErrorCode

pattern ERROR_DS_NONEXISTENT_MUST_HAVE  = 8388 :: SystemErrorCode

pattern ERROR_DS_AUX_CLS_TEST_FAIL      = 8389 :: SystemErrorCode

pattern ERROR_DS_NONEXISTENT_POSS_SUP   = 8390 :: SystemErrorCode

pattern ERROR_DS_SUB_CLS_TEST_FAIL      = 8391 :: SystemErrorCode

pattern ERROR_DS_BAD_RDN_ATT_ID_SYNTAX  = 8392 :: SystemErrorCode

pattern ERROR_DS_EXISTS_IN_AUX_CLS      = 8393 :: SystemErrorCode

pattern ERROR_DS_EXISTS_IN_SUB_CLS      = 8394 :: SystemErrorCode

pattern ERROR_DS_EXISTS_IN_POSS_SUP     = 8395 :: SystemErrorCode

pattern ERROR_DS_RECALCSCHEMA_FAILED    = 8396 :: SystemErrorCode

pattern ERROR_DS_TREE_DELETE_NOT_FINISHED= 8397 :: SystemErrorCode

pattern ERROR_DS_CANT_DELETE            = 8398 :: SystemErrorCode

pattern ERROR_DS_ATT_SCHEMA_REQ_ID      = 8399 :: SystemErrorCode

pattern ERROR_DS_BAD_ATT_SCHEMA_SYNTAX  = 8400 :: SystemErrorCode

pattern ERROR_DS_CANT_CACHE_ATT         = 8401 :: SystemErrorCode

pattern ERROR_DS_CANT_CACHE_CLASS       = 8402 :: SystemErrorCode

pattern ERROR_DS_CANT_REMOVE_ATT_CACHE  = 8403 :: SystemErrorCode

pattern ERROR_DS_CANT_REMOVE_CLASS_CACHE= 8404 :: SystemErrorCode

pattern ERROR_DS_CANT_RETRIEVE_DN       = 8405 :: SystemErrorCode

pattern ERROR_DS_MISSING_SUPREF         = 8406 :: SystemErrorCode

pattern ERROR_DS_CANT_RETRIEVE_INSTANCE = 8407 :: SystemErrorCode

pattern ERROR_DS_CODE_INCONSISTENCY     = 8408 :: SystemErrorCode

pattern ERROR_DS_DATABASE_ERROR         = 8409 :: SystemErrorCode

pattern ERROR_DS_GOVERNSID_MISSING      = 8410 :: SystemErrorCode

pattern ERROR_DS_MISSING_EXPECTED_ATT   = 8411 :: SystemErrorCode

pattern ERROR_DS_NCNAME_MISSING_CR_REF  = 8412 :: SystemErrorCode

pattern ERROR_DS_SECURITY_CHECKING_ERROR= 8413 :: SystemErrorCode

pattern ERROR_DS_SCHEMA_NOT_LOADED      = 8414 :: SystemErrorCode

pattern ERROR_DS_SCHEMA_ALLOC_FAILED    = 8415 :: SystemErrorCode

pattern ERROR_DS_ATT_SCHEMA_REQ_SYNTAX  = 8416 :: SystemErrorCode

pattern ERROR_DS_GCVERIFY_ERROR         = 8417 :: SystemErrorCode

pattern ERROR_DS_DRA_SCHEMA_MISMATCH    = 8418 :: SystemErrorCode

pattern ERROR_DS_CANT_FIND_DSA_OBJ      = 8419 :: SystemErrorCode

pattern ERROR_DS_CANT_FIND_EXPECTED_NC  = 8420 :: SystemErrorCode

pattern ERROR_DS_CANT_FIND_NC_IN_CACHE  = 8421 :: SystemErrorCode

pattern ERROR_DS_CANT_RETRIEVE_CHILD    = 8422 :: SystemErrorCode

pattern ERROR_DS_SECURITY_ILLEGAL_MODIFY= 8423 :: SystemErrorCode

pattern ERROR_DS_CANT_REPLACE_HIDDEN_REC= 8424 :: SystemErrorCode

pattern ERROR_DS_BAD_HIERARCHY_FILE     = 8425 :: SystemErrorCode

pattern ERROR_DS_BUILD_HIERARCHY_TABLE_FAILED= 8426 :: SystemErrorCode

pattern ERROR_DS_CONFIG_PARAM_MISSING   = 8427 :: SystemErrorCode

pattern ERROR_DS_COUNTING_AB_INDICES_FAILED= 8428 :: SystemErrorCode

pattern ERROR_DS_HIERARCHY_TABLE_MALLOC_FAILED= 8429 :: SystemErrorCode

pattern ERROR_DS_INTERNAL_FAILURE       = 8430 :: SystemErrorCode

pattern ERROR_DS_UNKNOWN_ERROR          = 8431 :: SystemErrorCode

pattern ERROR_DS_ROOT_REQUIRES_CLASS_TOP= 8432 :: SystemErrorCode

pattern ERROR_DS_REFUSING_FSMO_ROLES    = 8433 :: SystemErrorCode

pattern ERROR_DS_MISSING_FSMO_SETTINGS  = 8434 :: SystemErrorCode

pattern ERROR_DS_UNABLE_TO_SURRENDER_ROLES= 8435 :: SystemErrorCode

pattern ERROR_DS_DRA_GENERIC            = 8436 :: SystemErrorCode

pattern ERROR_DS_DRA_INVALID_PARAMETER  = 8437 :: SystemErrorCode

pattern ERROR_DS_DRA_BUSY               = 8438 :: SystemErrorCode

pattern ERROR_DS_DRA_BAD_DN             = 8439 :: SystemErrorCode

pattern ERROR_DS_DRA_BAD_NC             = 8440 :: SystemErrorCode

pattern ERROR_DS_DRA_DN_EXISTS          = 8441 :: SystemErrorCode

pattern ERROR_DS_DRA_INTERNAL_ERROR     = 8442 :: SystemErrorCode

pattern ERROR_DS_DRA_INCONSISTENT_DIT   = 8443 :: SystemErrorCode

pattern ERROR_DS_DRA_CONNECTION_FAILED  = 8444 :: SystemErrorCode

pattern ERROR_DS_DRA_BAD_INSTANCE_TYPE  = 8445 :: SystemErrorCode

pattern ERROR_DS_DRA_OUT_OF_MEM         = 8446 :: SystemErrorCode

pattern ERROR_DS_DRA_MAIL_PROBLEM       = 8447 :: SystemErrorCode

pattern ERROR_DS_DRA_REF_ALREADY_EXISTS = 8448 :: SystemErrorCode

pattern ERROR_DS_DRA_REF_NOT_FOUND      = 8449 :: SystemErrorCode

pattern ERROR_DS_DRA_OBJ_IS_REP_SOURCE  = 8450 :: SystemErrorCode

pattern ERROR_DS_DRA_DB_ERROR           = 8451 :: SystemErrorCode

pattern ERROR_DS_DRA_NO_REPLICA         = 8452 :: SystemErrorCode

pattern ERROR_DS_DRA_ACCESS_DENIED      = 8453 :: SystemErrorCode

pattern ERROR_DS_DRA_NOT_SUPPORTED      = 8454 :: SystemErrorCode

pattern ERROR_DS_DRA_RPC_CANCELLED      = 8455 :: SystemErrorCode

pattern ERROR_DS_DRA_SOURCE_DISABLED    = 8456 :: SystemErrorCode

pattern ERROR_DS_DRA_SINK_DISABLED      = 8457 :: SystemErrorCode

pattern ERROR_DS_DRA_NAME_COLLISION     = 8458 :: SystemErrorCode

pattern ERROR_DS_DRA_SOURCE_REINSTALLED = 8459 :: SystemErrorCode

pattern ERROR_DS_DRA_MISSING_PARENT     = 8460 :: SystemErrorCode

pattern ERROR_DS_DRA_PREEMPTED          = 8461 :: SystemErrorCode

pattern ERROR_DS_DRA_ABANDON_SYNC       = 8462 :: SystemErrorCode

pattern ERROR_DS_DRA_SHUTDOWN           = 8463 :: SystemErrorCode

pattern ERROR_DS_DRA_INCOMPATIBLE_PARTIAL_SET= 8464 :: SystemErrorCode

pattern ERROR_DS_DRA_SOURCE_IS_PARTIAL_REPLICA= 8465 :: SystemErrorCode

pattern ERROR_DS_DRA_EXTN_CONNECTION_FAILED= 8466 :: SystemErrorCode

pattern ERROR_DS_INSTALL_SCHEMA_MISMATCH= 8467 :: SystemErrorCode

pattern ERROR_DS_DUP_LINK_ID            = 8468 :: SystemErrorCode

pattern ERROR_DS_NAME_ERROR_RESOLVING   = 8469 :: SystemErrorCode

pattern ERROR_DS_NAME_ERROR_NOT_FOUND   = 8470 :: SystemErrorCode

pattern ERROR_DS_NAME_ERROR_NOT_UNIQUE  = 8471 :: SystemErrorCode

pattern ERROR_DS_NAME_ERROR_NO_MAPPING  = 8472 :: SystemErrorCode

pattern ERROR_DS_NAME_ERROR_DOMAIN_ONLY = 8473 :: SystemErrorCode

pattern ERROR_DS_NAME_ERROR_NO_SYNTACTICAL_MAPPING= 8474 :: SystemErrorCode

pattern ERROR_DS_CONSTRUCTED_ATT_MOD    = 8475 :: SystemErrorCode

pattern ERROR_DS_WRONG_OM_OBJ_CLASS     = 8476 :: SystemErrorCode

pattern ERROR_DS_DRA_REPL_PENDING       = 8477 :: SystemErrorCode

pattern ERROR_DS_DS_REQUIRED            = 8478 :: SystemErrorCode

pattern ERROR_DS_INVALID_LDAP_DISPLAY_NAME= 8479 :: SystemErrorCode

pattern ERROR_DS_NON_BASE_SEARCH        = 8480 :: SystemErrorCode

pattern ERROR_DS_CANT_RETRIEVE_ATTS     = 8481 :: SystemErrorCode

pattern ERROR_DS_BACKLINK_WITHOUT_LINK  = 8482 :: SystemErrorCode

pattern ERROR_DS_EPOCH_MISMATCH         = 8483 :: SystemErrorCode

pattern ERROR_DS_SRC_NAME_MISMATCH      = 8484 :: SystemErrorCode

pattern ERROR_DS_SRC_AND_DST_NC_IDENTICAL= 8485 :: SystemErrorCode

pattern ERROR_DS_DST_NC_MISMATCH        = 8486 :: SystemErrorCode

pattern ERROR_DS_NOT_AUTHORITIVE_FOR_DST_NC= 8487 :: SystemErrorCode

pattern ERROR_DS_SRC_GUID_MISMATCH      = 8488 :: SystemErrorCode

pattern ERROR_DS_CANT_MOVE_DELETED_OBJECT= 8489 :: SystemErrorCode

pattern ERROR_DS_PDC_OPERATION_IN_PROGRESS= 8490 :: SystemErrorCode

pattern ERROR_DS_CROSS_DOMAIN_CLEANUP_REQD= 8491 :: SystemErrorCode

pattern ERROR_DS_ILLEGAL_XDOM_MOVE_OPERATION= 8492 :: SystemErrorCode

pattern ERROR_DS_CANT_WITH_ACCT_GROUP_MEMBERSHPS= 8493 :: SystemErrorCode

pattern ERROR_DS_NC_MUST_HAVE_NC_PARENT = 8494 :: SystemErrorCode

pattern ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE= 8495 :: SystemErrorCode

pattern ERROR_DS_DST_DOMAIN_NOT_NATIVE  = 8496 :: SystemErrorCode

pattern ERROR_DS_MISSING_INFRASTRUCTURE_CONTAINER= 8497 :: SystemErrorCode

pattern ERROR_DS_CANT_MOVE_ACCOUNT_GROUP= 8498 :: SystemErrorCode

pattern ERROR_DS_CANT_MOVE_RESOURCE_GROUP= 8499 :: SystemErrorCode

pattern ERROR_DS_INVALID_SEARCH_FLAG    = 8500 :: SystemErrorCode

pattern ERROR_DS_NO_TREE_DELETE_ABOVE_NC= 8501 :: SystemErrorCode

pattern ERROR_DS_COULDNT_LOCK_TREE_FOR_DELETE= 8502 :: SystemErrorCode

pattern ERROR_DS_COULDNT_IDENTIFY_OBJECTS_FOR_TREE_DELETE= 8503 :: SystemErrorCode

pattern ERROR_DS_SAM_INIT_FAILURE       = 8504 :: SystemErrorCode

pattern ERROR_DS_SENSITIVE_GROUP_VIOLATION= 8505 :: SystemErrorCode

pattern ERROR_DS_CANT_MOD_PRIMARYGROUPID= 8506 :: SystemErrorCode

pattern ERROR_DS_ILLEGAL_BASE_SCHEMA_MOD= 8507 :: SystemErrorCode

pattern ERROR_DS_NONSAFE_SCHEMA_CHANGE  = 8508 :: SystemErrorCode

pattern ERROR_DS_SCHEMA_UPDATE_DISALLOWED= 8509 :: SystemErrorCode

pattern ERROR_DS_CANT_CREATE_UNDER_SCHEMA= 8510 :: SystemErrorCode

pattern ERROR_DS_INSTALL_NO_SRC_SCH_VERSION= 8511 :: SystemErrorCode

pattern ERROR_DS_INSTALL_NO_SCH_VERSION_IN_INIFILE= 8512 :: SystemErrorCode

pattern ERROR_DS_INVALID_GROUP_TYPE     = 8513 :: SystemErrorCode

pattern ERROR_DS_NO_NEST_GLOBALGROUP_IN_MIXEDDOMAIN= 8514 :: SystemErrorCode

pattern ERROR_DS_NO_NEST_LOCALGROUP_IN_MIXEDDOMAIN= 8515 :: SystemErrorCode

pattern ERROR_DS_GLOBAL_CANT_HAVE_LOCAL_MEMBER= 8516 :: SystemErrorCode

pattern ERROR_DS_GLOBAL_CANT_HAVE_UNIVERSAL_MEMBER= 8517 :: SystemErrorCode

pattern ERROR_DS_UNIVERSAL_CANT_HAVE_LOCAL_MEMBER= 8518 :: SystemErrorCode

pattern ERROR_DS_GLOBAL_CANT_HAVE_CROSSDOMAIN_MEMBER= 8519 :: SystemErrorCode

pattern ERROR_DS_LOCAL_CANT_HAVE_CROSSDOMAIN_LOCAL_MEMBER= 8520 :: SystemErrorCode

pattern ERROR_DS_HAVE_PRIMARY_MEMBERS   = 8521 :: SystemErrorCode

pattern ERROR_DS_STRING_SD_CONVERSION_FAILED= 8522 :: SystemErrorCode

pattern ERROR_DS_NAMING_MASTER_GC       = 8523 :: SystemErrorCode

pattern ERROR_DS_DNS_LOOKUP_FAILURE     = 8524 :: SystemErrorCode

pattern ERROR_DS_COULDNT_UPDATE_SPNS    = 8525 :: SystemErrorCode

pattern ERROR_DS_CANT_RETRIEVE_SD       = 8526 :: SystemErrorCode

pattern ERROR_DS_KEY_NOT_UNIQUE         = 8527 :: SystemErrorCode

pattern ERROR_DS_WRONG_LINKED_ATT_SYNTAX= 8528 :: SystemErrorCode

pattern ERROR_DS_SAM_NEED_BOOTKEY_PASSWORD= 8529 :: SystemErrorCode

pattern ERROR_DS_SAM_NEED_BOOTKEY_FLOPPY= 8530 :: SystemErrorCode

pattern ERROR_DS_CANT_START             = 8531 :: SystemErrorCode

pattern ERROR_DS_INIT_FAILURE           = 8532 :: SystemErrorCode

pattern ERROR_DS_NO_PKT_PRIVACY_ON_CONNECTION= 8533 :: SystemErrorCode

pattern ERROR_DS_SOURCE_DOMAIN_IN_FOREST= 8534 :: SystemErrorCode

pattern ERROR_DS_DESTINATION_DOMAIN_NOT_IN_FOREST= 8535 :: SystemErrorCode

pattern ERROR_DS_DESTINATION_AUDITING_NOT_ENABLED= 8536 :: SystemErrorCode

pattern ERROR_DS_CANT_FIND_DC_FOR_SRC_DOMAIN= 8537 :: SystemErrorCode

pattern ERROR_DS_SRC_OBJ_NOT_GROUP_OR_USER= 8538 :: SystemErrorCode

pattern ERROR_DS_SRC_SID_EXISTS_IN_FOREST= 8539 :: SystemErrorCode

pattern ERROR_DS_SRC_AND_DST_OBJECT_CLASS_MISMATCH= 8540 :: SystemErrorCode

pattern ERROR_SAM_INIT_FAILURE          = 8541 :: SystemErrorCode

pattern ERROR_DS_DRA_SCHEMA_INFO_SHIP   = 8542 :: SystemErrorCode

pattern ERROR_DS_DRA_SCHEMA_CONFLICT    = 8543 :: SystemErrorCode

pattern ERROR_DS_DRA_EARLIER_SCHEMA_CONFLICT= 8544 :: SystemErrorCode

pattern ERROR_DS_DRA_OBJ_NC_MISMATCH    = 8545 :: SystemErrorCode

pattern ERROR_DS_NC_STILL_HAS_DSAS      = 8546 :: SystemErrorCode

pattern ERROR_DS_GC_REQUIRED            = 8547 :: SystemErrorCode

pattern ERROR_DS_LOCAL_MEMBER_OF_LOCAL_ONLY= 8548 :: SystemErrorCode

pattern ERROR_DS_NO_FPO_IN_UNIVERSAL_GROUPS= 8549 :: SystemErrorCode

pattern ERROR_DS_CANT_ADD_TO_GC         = 8550 :: SystemErrorCode

pattern ERROR_DS_NO_CHECKPOINT_WITH_PDC = 8551 :: SystemErrorCode

pattern ERROR_DS_SOURCE_AUDITING_NOT_ENABLED= 8552 :: SystemErrorCode

pattern ERROR_DS_CANT_CREATE_IN_NONDOMAIN_NC= 8553 :: SystemErrorCode

pattern ERROR_DS_INVALID_NAME_FOR_SPN   = 8554 :: SystemErrorCode

pattern ERROR_DS_FILTER_USES_CONTRUCTED_ATTRS= 8555 :: SystemErrorCode

pattern ERROR_DS_UNICODEPWD_NOT_IN_QUOTES= 8556 :: SystemErrorCode

pattern ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED= 8557 :: SystemErrorCode

pattern ERROR_DS_MUST_BE_RUN_ON_DST_DC  = 8558 :: SystemErrorCode

pattern ERROR_DS_SRC_DC_MUST_BE_SP4_OR_GREATER= 8559 :: SystemErrorCode

pattern ERROR_DS_CANT_TREE_DELETE_CRITICAL_OBJ= 8560 :: SystemErrorCode

pattern ERROR_DS_INIT_FAILURE_CONSOLE   = 8561 :: SystemErrorCode

pattern ERROR_DS_SAM_INIT_FAILURE_CONSOLE= 8562 :: SystemErrorCode

pattern ERROR_DS_FOREST_VERSION_TOO_HIGH= 8563 :: SystemErrorCode

pattern ERROR_DS_DOMAIN_VERSION_TOO_HIGH= 8564 :: SystemErrorCode

pattern ERROR_DS_FOREST_VERSION_TOO_LOW = 8565 :: SystemErrorCode

pattern ERROR_DS_DOMAIN_VERSION_TOO_LOW = 8566 :: SystemErrorCode

pattern ERROR_DS_INCOMPATIBLE_VERSION   = 8567 :: SystemErrorCode

pattern ERROR_DS_LOW_DSA_VERSION        = 8568 :: SystemErrorCode

pattern ERROR_DS_NO_BEHAVIOR_VERSION_IN_MIXEDDOMAIN= 8569 :: SystemErrorCode

pattern ERROR_DS_NOT_SUPPORTED_SORT_ORDER= 8570 :: SystemErrorCode

pattern ERROR_DS_NAME_NOT_UNIQUE        = 8571 :: SystemErrorCode

pattern ERROR_DS_MACHINE_ACCOUNT_CREATED_PRENT4= 8572 :: SystemErrorCode

pattern ERROR_DS_OUT_OF_VERSION_STORE   = 8573 :: SystemErrorCode

pattern ERROR_DS_INCOMPATIBLE_CONTROLS_USED= 8574 :: SystemErrorCode

pattern ERROR_DS_NO_REF_DOMAIN          = 8575 :: SystemErrorCode

pattern ERROR_DS_RESERVED_LINK_ID       = 8576 :: SystemErrorCode

pattern ERROR_DS_LINK_ID_NOT_AVAILABLE  = 8577 :: SystemErrorCode

pattern ERROR_DS_AG_CANT_HAVE_UNIVERSAL_MEMBER= 8578 :: SystemErrorCode

pattern ERROR_DS_MODIFYDN_DISALLOWED_BY_INSTANCE_TYPE= 8579 :: SystemErrorCode

pattern ERROR_DS_NO_OBJECT_MOVE_IN_SCHEMA_NC= 8580 :: SystemErrorCode

pattern ERROR_DS_MODIFYDN_DISALLOWED_BY_FLAG= 8581 :: SystemErrorCode

pattern ERROR_DS_MODIFYDN_WRONG_GRANDPARENT= 8582 :: SystemErrorCode

pattern ERROR_DS_NAME_ERROR_TRUST_REFERRAL= 8583 :: SystemErrorCode

pattern ERROR_NOT_SUPPORTED_ON_STANDARD_SERVER= 8584 :: SystemErrorCode

pattern ERROR_DS_CANT_ACCESS_REMOTE_PART_OF_AD= 8585 :: SystemErrorCode

pattern ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE_V2= 8586 :: SystemErrorCode

pattern ERROR_DS_THREAD_LIMIT_EXCEEDED  = 8587 :: SystemErrorCode

pattern ERROR_DS_NOT_CLOSEST            = 8588 :: SystemErrorCode

pattern ERROR_DS_CANT_DERIVE_SPN_WITHOUT_SERVER_REF= 8589 :: SystemErrorCode

pattern ERROR_DS_SINGLE_USER_MODE_FAILED= 8590 :: SystemErrorCode

pattern ERROR_DS_NTDSCRIPT_SYNTAX_ERROR = 8591 :: SystemErrorCode

pattern ERROR_DS_NTDSCRIPT_PROCESS_ERROR= 8592 :: SystemErrorCode

pattern ERROR_DS_DIFFERENT_REPL_EPOCHS  = 8593 :: SystemErrorCode

pattern ERROR_DS_DRS_EXTENSIONS_CHANGED = 8594 :: SystemErrorCode

pattern ERROR_DS_REPLICA_SET_CHANGE_NOT_ALLOWED_ON_DISABLED_CR= 8595 :: SystemErrorCode

pattern ERROR_DS_NO_MSDS_INTID          = 8596 :: SystemErrorCode

pattern ERROR_DS_DUP_MSDS_INTID         = 8597 :: SystemErrorCode

pattern ERROR_DS_EXISTS_IN_RDNATTID     = 8598 :: SystemErrorCode

pattern ERROR_DS_AUTHORIZATION_FAILED   = 8599 :: SystemErrorCode

pattern ERROR_DS_INVALID_SCRIPT         = 8600 :: SystemErrorCode

pattern ERROR_DS_REMOTE_CROSSREF_OP_FAILED= 8601 :: SystemErrorCode

pattern ERROR_DS_CROSS_REF_BUSY         = 8602 :: SystemErrorCode

pattern ERROR_DS_CANT_DERIVE_SPN_FOR_DELETED_DOMAIN= 8603 :: SystemErrorCode

pattern ERROR_DS_CANT_DEMOTE_WITH_WRITEABLE_NC= 8604 :: SystemErrorCode

pattern ERROR_DS_DUPLICATE_ID_FOUND     = 8605 :: SystemErrorCode

pattern ERROR_DS_INSUFFICIENT_ATTR_TO_CREATE_OBJECT= 8606 :: SystemErrorCode

pattern ERROR_DS_GROUP_CONVERSION_ERROR = 8607 :: SystemErrorCode

pattern ERROR_DS_CANT_MOVE_APP_BASIC_GROUP= 8608 :: SystemErrorCode

pattern ERROR_DS_CANT_MOVE_APP_QUERY_GROUP= 8609 :: SystemErrorCode

pattern ERROR_DS_ROLE_NOT_VERIFIED      = 8610 :: SystemErrorCode

pattern ERROR_DS_WKO_CONTAINER_CANNOT_BE_SPECIAL= 8611 :: SystemErrorCode

pattern ERROR_DS_DOMAIN_RENAME_IN_PROGRESS= 8612 :: SystemErrorCode

pattern ERROR_DS_EXISTING_AD_CHILD_NC   = 8613 :: SystemErrorCode

pattern ERROR_DS_REPL_LIFETIME_EXCEEDED = 8614 :: SystemErrorCode

pattern ERROR_DS_DISALLOWED_IN_SYSTEM_CONTAINER= 8615 :: SystemErrorCode

pattern ERROR_DS_LDAP_SEND_QUEUE_FULL   = 8616 :: SystemErrorCode

pattern ERROR_DS_DRA_OUT_SCHEDULE_WINDOW= 8617 :: SystemErrorCode

pattern ERROR_DS_POLICY_NOT_KNOWN       = 8618 :: SystemErrorCode

pattern ERROR_NO_SITE_SETTINGS_OBJECT   = 8619 :: SystemErrorCode

pattern ERROR_NO_SECRETS                = 8620 :: SystemErrorCode

pattern ERROR_NO_WRITABLE_DC_FOUND      = 8621 :: SystemErrorCode

pattern ERROR_DS_NO_SERVER_OBJECT       = 8622 :: SystemErrorCode

pattern ERROR_DS_NO_NTDSA_OBJECT        = 8623 :: SystemErrorCode

pattern ERROR_DS_NON_ASQ_SEARCH         = 8624 :: SystemErrorCode

pattern ERROR_DS_AUDIT_FAILURE          = 8625 :: SystemErrorCode

pattern ERROR_DS_INVALID_SEARCH_FLAG_SUBTREE= 8626 :: SystemErrorCode

pattern ERROR_DS_INVALID_SEARCH_FLAG_TUPLE= 8627 :: SystemErrorCode

pattern ERROR_DS_HIERARCHY_TABLE_TOO_DEEP= 8628 :: SystemErrorCode

pattern ERROR_DS_DRA_CORRUPT_UTD_VECTOR = 8629 :: SystemErrorCode

pattern ERROR_DS_DRA_SECRETS_DENIED     = 8630 :: SystemErrorCode

pattern ERROR_DS_RESERVED_MAPI_ID       = 8631 :: SystemErrorCode

pattern ERROR_DS_MAPI_ID_NOT_AVAILABLE  = 8632 :: SystemErrorCode

pattern ERROR_DS_DRA_MISSING_KRBTGT_SECRET= 8633 :: SystemErrorCode

pattern ERROR_DS_DOMAIN_NAME_EXISTS_IN_FOREST= 8634 :: SystemErrorCode

pattern ERROR_DS_FLAT_NAME_EXISTS_IN_FOREST= 8635 :: SystemErrorCode

pattern ERROR_INVALID_USER_PRINCIPAL_NAME= 8636 :: SystemErrorCode

pattern ERROR_DS_OID_MAPPED_GROUP_CANT_HAVE_MEMBERS= 8637 :: SystemErrorCode

pattern ERROR_DS_OID_NOT_FOUND          = 8638 :: SystemErrorCode

pattern ERROR_DS_DRA_RECYCLED_TARGET    = 8639 :: SystemErrorCode

pattern ERROR_DS_DISALLOWED_NC_REDIRECT = 8640 :: SystemErrorCode

pattern ERROR_DS_HIGH_ADLDS_FFL         = 8641 :: SystemErrorCode

pattern ERROR_DS_HIGH_DSA_VERSION       = 8642 :: SystemErrorCode

pattern ERROR_DS_LOW_ADLDS_FFL          = 8643 :: SystemErrorCode

pattern ERROR_DOMAIN_SID_SAME_AS_LOCAL_WORKSTATION= 8644 :: SystemErrorCode

pattern ERROR_DS_UNDELETE_SAM_VALIDATION_FAILED= 8645 :: SystemErrorCode

pattern ERROR_INCORRECT_ACCOUNT_TYPE    = 8646 :: SystemErrorCode

pattern ERROR_DS_SPN_VALUE_NOT_UNIQUE_IN_FOREST= 8647 :: SystemErrorCode

pattern ERROR_DS_UPN_VALUE_NOT_UNIQUE_IN_FOREST= 8648 :: SystemErrorCode

pattern ERROR_IPSEC_QM_POLICY_EXISTS    = 13000 :: SystemErrorCode

pattern ERROR_IPSEC_QM_POLICY_NOT_FOUND = 13001 :: SystemErrorCode

pattern ERROR_IPSEC_QM_POLICY_IN_USE    = 13002 :: SystemErrorCode

pattern ERROR_IPSEC_MM_POLICY_EXISTS    = 13003 :: SystemErrorCode

pattern ERROR_IPSEC_MM_POLICY_NOT_FOUND = 13004 :: SystemErrorCode

pattern ERROR_IPSEC_MM_POLICY_IN_USE    = 13005 :: SystemErrorCode

pattern ERROR_IPSEC_MM_FILTER_EXISTS    = 13006 :: SystemErrorCode

pattern ERROR_IPSEC_MM_FILTER_NOT_FOUND = 13007 :: SystemErrorCode

pattern ERROR_IPSEC_TRANSPORT_FILTER_EXISTS= 13008 :: SystemErrorCode

pattern ERROR_IPSEC_TRANSPORT_FILTER_NOT_FOUND= 13009 :: SystemErrorCode

pattern ERROR_IPSEC_MM_AUTH_EXISTS      = 13010 :: SystemErrorCode

pattern ERROR_IPSEC_MM_AUTH_NOT_FOUND   = 13011 :: SystemErrorCode

pattern ERROR_IPSEC_MM_AUTH_IN_USE      = 13012 :: SystemErrorCode

pattern ERROR_IPSEC_DEFAULT_MM_POLICY_NOT_FOUND= 13013 :: SystemErrorCode

pattern ERROR_IPSEC_DEFAULT_MM_AUTH_NOT_FOUND= 13014 :: SystemErrorCode

pattern ERROR_IPSEC_DEFAULT_QM_POLICY_NOT_FOUND= 13015 :: SystemErrorCode

pattern ERROR_IPSEC_TUNNEL_FILTER_EXISTS= 13016 :: SystemErrorCode

pattern ERROR_IPSEC_TUNNEL_FILTER_NOT_FOUND= 13017 :: SystemErrorCode

pattern ERROR_IPSEC_MM_FILTER_PENDING_DELETION= 13018 :: SystemErrorCode

pattern ERROR_IPSEC_TRANSPORT_FILTER_PENDING_DELETION= 13019 :: SystemErrorCode

pattern ERROR_IPSEC_TUNNEL_FILTER_PENDING_DELETION= 13020 :: SystemErrorCode

pattern ERROR_IPSEC_MM_POLICY_PENDING_DELETION= 13021 :: SystemErrorCode

pattern ERROR_IPSEC_MM_AUTH_PENDING_DELETION= 13022 :: SystemErrorCode

pattern ERROR_IPSEC_QM_POLICY_PENDING_DELETION= 13023 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NEG_STATUS_BEGIN= 13800 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_AUTH_FAIL       = 13801 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_ATTRIB_FAIL     = 13802 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NEGOTIATION_PENDING= 13803 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_GENERAL_PROCESSING_ERROR= 13804 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_TIMED_OUT       = 13805 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NO_CERT         = 13806 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SA_DELETED      = 13807 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SA_REAPED       = 13808 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_MM_ACQUIRE_DROP = 13809 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_QM_ACQUIRE_DROP = 13810 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_QUEUE_DROP_MM   = 13811 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_QUEUE_DROP_NO_MM= 13812 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_DROP_NO_RESPONSE= 13813 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_MM_DELAY_DROP   = 13814 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_QM_DELAY_DROP   = 13815 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_ERROR           = 13816 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_CRL_FAILED      = 13817 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_KEY_USAGE= 13818 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_CERT_TYPE= 13819 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NO_PRIVATE_KEY  = 13820 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SIMULTANEOUS_REKEY= 13821 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_DH_FAIL         = 13822 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_CRITICAL_PAYLOAD_NOT_RECOGNIZED= 13823 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_HEADER  = 13824 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NO_POLICY       = 13825 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_SIGNATURE= 13826 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_KERBEROS_ERROR  = 13827 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NO_PUBLIC_KEY   = 13828 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR     = 13829 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_SA  = 13830 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_PROP= 13831 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_TRANS= 13832 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_KE  = 13833 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_ID  = 13834 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_CERT= 13835 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_CERT_REQ= 13836 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_HASH= 13837 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_SIG = 13838 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_NONCE= 13839 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_NOTIFY= 13840 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_DELETE= 13841 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_VENDOR= 13842 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_PAYLOAD = 13843 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_LOAD_SOFT_SA    = 13844 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SOFT_SA_TORN_DOWN= 13845 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_COOKIE  = 13846 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NO_PEER_CERT    = 13847 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PEER_CRL_FAILED = 13848 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_POLICY_CHANGE   = 13849 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NO_MM_POLICY    = 13850 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NOTCBPRIV       = 13851 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SECLOADFAIL     = 13852 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_FAILSSPINIT     = 13853 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_FAILQUERYSSP    = 13854 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SRVACQFAIL      = 13855 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SRVQUERYCRED    = 13856 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_GETSPIFAIL      = 13857 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_FILTER  = 13858 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_OUT_OF_MEMORY   = 13859 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_ADD_UPDATE_KEY_FAILED= 13860 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_POLICY  = 13861 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_UNKNOWN_DOI     = 13862 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_SITUATION= 13863 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_DH_FAILURE      = 13864 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_GROUP   = 13865 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_ENCRYPT         = 13866 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_DECRYPT         = 13867 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_POLICY_MATCH    = 13868 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_UNSUPPORTED_ID  = 13869 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_HASH    = 13870 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_HASH_ALG= 13871 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_HASH_SIZE= 13872 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_ENCRYPT_ALG= 13873 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_AUTH_ALG= 13874 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_SIG     = 13875 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_LOAD_FAILED     = 13876 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_RPC_DELETE      = 13877 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_BENIGN_REINIT   = 13878 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_RESPONDER_LIFETIME_NOTIFY= 13879 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_MAJOR_VERSION= 13880 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_CERT_KEYLEN= 13881 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_MM_LIMIT        = 13882 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NEGOTIATION_DISABLED= 13883 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_QM_LIMIT        = 13884 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_MM_EXPIRED      = 13885 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PEER_MM_ASSUMED_INVALID= 13886 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_CERT_CHAIN_POLICY_MISMATCH= 13887 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_UNEXPECTED_MESSAGE_ID= 13888 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_AUTH_PAYLOAD= 13889 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_DOS_COOKIE_SENT = 13890 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_SHUTTING_DOWN   = 13891 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_CGA_AUTH_FAILED = 13892 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PROCESS_ERR_NATOA= 13893 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INVALID_MM_FOR_QM= 13894 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_QM_EXPIRED      = 13895 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_TOO_MANY_FILTERS= 13896 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NEG_STATUS_END  = 13897 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_KILL_DUMMY_NAP_TUNNEL= 13898 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_INNER_IP_ASSIGNMENT_FAILURE= 13899 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_REQUIRE_CP_PAYLOAD_MISSING= 13900 :: SystemErrorCode

pattern ERROR_IPSEC_KEY_MODULE_IMPERSONATION_NEGOTIATION_PENDING= 13901 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_COEXISTENCE_SUPPRESS= 13902 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_RATELIMIT_DROP  = 13903 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_PEER_DOESNT_SUPPORT_MOBIKE= 13904 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_AUTHORIZATION_FAILURE= 13905 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_STRONG_CRED_AUTHORIZATION_FAILURE= 13906 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_AUTHORIZATION_FAILURE_WITH_OPTIONAL_RETRY= 13907 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_STRONG_CRED_AUTHORIZATION_AND_CERTMAP_FAILURE= 13908 :: SystemErrorCode

pattern ERROR_IPSEC_IKE_NEG_STATUS_EXTENDED_END= 13909 :: SystemErrorCode

pattern ERROR_IPSEC_BAD_SPI             = 13910 :: SystemErrorCode

pattern ERROR_IPSEC_SA_LIFETIME_EXPIRED = 13911 :: SystemErrorCode

pattern ERROR_IPSEC_WRONG_SA            = 13912 :: SystemErrorCode

pattern ERROR_IPSEC_REPLAY_CHECK_FAILED = 13913 :: SystemErrorCode

pattern ERROR_IPSEC_INVALID_PACKET      = 13914 :: SystemErrorCode

pattern ERROR_IPSEC_INTEGRITY_CHECK_FAILED= 13915 :: SystemErrorCode

pattern ERROR_IPSEC_CLEAR_TEXT_DROP     = 13916 :: SystemErrorCode

pattern ERROR_IPSEC_AUTH_FIREWALL_DROP  = 13917 :: SystemErrorCode

pattern ERROR_IPSEC_THROTTLE_DROP       = 13918 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_BLOCK          = 13925 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_RECEIVED_MULTICAST= 13926 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_INVALID_PACKET = 13927 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_STATE_LOOKUP_FAILED= 13928 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_MAX_ENTRIES    = 13929 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_KEYMOD_NOT_ALLOWED= 13930 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_NOT_INSTALLED  = 13931 :: SystemErrorCode

pattern ERROR_IPSEC_DOSP_MAX_PER_IP_RATELIMIT_QUEUES= 13932 :: SystemErrorCode

pattern ERROR_SXS_SECTION_NOT_FOUND     = 14000 :: SystemErrorCode

pattern ERROR_SXS_CANT_GEN_ACTCTX       = 14001 :: SystemErrorCode

pattern ERROR_SXS_INVALID_ACTCTXDATA_FORMAT= 14002 :: SystemErrorCode

pattern ERROR_SXS_ASSEMBLY_NOT_FOUND    = 14003 :: SystemErrorCode

pattern ERROR_SXS_MANIFEST_FORMAT_ERROR = 14004 :: SystemErrorCode

pattern ERROR_SXS_MANIFEST_PARSE_ERROR  = 14005 :: SystemErrorCode

pattern ERROR_SXS_ACTIVATION_CONTEXT_DISABLED= 14006 :: SystemErrorCode

pattern ERROR_SXS_KEY_NOT_FOUND         = 14007 :: SystemErrorCode

pattern ERROR_SXS_VERSION_CONFLICT      = 14008 :: SystemErrorCode

pattern ERROR_SXS_WRONG_SECTION_TYPE    = 14009 :: SystemErrorCode

pattern ERROR_SXS_THREAD_QUERIES_DISABLED= 14010 :: SystemErrorCode

pattern ERROR_SXS_PROCESS_DEFAULT_ALREADY_SET= 14011 :: SystemErrorCode

pattern ERROR_SXS_UNKNOWN_ENCODING_GROUP= 14012 :: SystemErrorCode

pattern ERROR_SXS_UNKNOWN_ENCODING      = 14013 :: SystemErrorCode

pattern ERROR_SXS_INVALID_XML_NAMESPACE_URI= 14014 :: SystemErrorCode

pattern ERROR_SXS_ROOT_MANIFEST_DEPENDENCY_NOT_INSTALLED= 14015 :: SystemErrorCode

pattern ERROR_SXS_LEAF_MANIFEST_DEPENDENCY_NOT_INSTALLED= 14016 :: SystemErrorCode

pattern ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE= 14017 :: SystemErrorCode

pattern ERROR_SXS_MANIFEST_MISSING_REQUIRED_DEFAULT_NAMESPACE= 14018 :: SystemErrorCode

pattern ERROR_SXS_MANIFEST_INVALID_REQUIRED_DEFAULT_NAMESPACE= 14019 :: SystemErrorCode

pattern ERROR_SXS_PRIVATE_MANIFEST_CROSS_PATH_WITH_REPARSE_POINT= 14020 :: SystemErrorCode

pattern ERROR_SXS_DUPLICATE_DLL_NAME    = 14021 :: SystemErrorCode

pattern ERROR_SXS_DUPLICATE_WINDOWCLASS_NAME= 14022 :: SystemErrorCode

pattern ERROR_SXS_DUPLICATE_CLSID       = 14023 :: SystemErrorCode

pattern ERROR_SXS_DUPLICATE_IID         = 14024 :: SystemErrorCode

pattern ERROR_SXS_DUPLICATE_TLBID       = 14025 :: SystemErrorCode

pattern ERROR_SXS_DUPLICATE_PROGID      = 14026 :: SystemErrorCode

pattern ERROR_SXS_DUPLICATE_ASSEMBLY_NAME= 14027 :: SystemErrorCode

pattern ERROR_SXS_FILE_HASH_MISMATCH    = 14028 :: SystemErrorCode

pattern ERROR_SXS_POLICY_PARSE_ERROR    = 14029 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MISSINGQUOTE    = 14030 :: SystemErrorCode

pattern ERROR_SXS_XML_E_COMMENTSYNTAX   = 14031 :: SystemErrorCode

pattern ERROR_SXS_XML_E_BADSTARTNAMECHAR= 14032 :: SystemErrorCode

pattern ERROR_SXS_XML_E_BADNAMECHAR     = 14033 :: SystemErrorCode

pattern ERROR_SXS_XML_E_BADCHARINSTRING = 14034 :: SystemErrorCode

pattern ERROR_SXS_XML_E_XMLDECLSYNTAX   = 14035 :: SystemErrorCode

pattern ERROR_SXS_XML_E_BADCHARDATA     = 14036 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MISSINGWHITESPACE= 14037 :: SystemErrorCode

pattern ERROR_SXS_XML_E_EXPECTINGTAGEND = 14038 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MISSINGSEMICOLON= 14039 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNBALANCEDPAREN = 14040 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INTERNALERROR   = 14041 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNEXPECTED_WHITESPACE= 14042 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INCOMPLETE_ENCODING= 14043 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MISSING_PAREN   = 14044 :: SystemErrorCode

pattern ERROR_SXS_XML_E_EXPECTINGCLOSEQUOTE= 14045 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MULTIPLE_COLONS = 14046 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALID_DECIMAL = 14047 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALID_HEXIDECIMAL= 14048 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALID_UNICODE = 14049 :: SystemErrorCode

pattern ERROR_SXS_XML_E_WHITESPACEORQUESTIONMARK= 14050 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNEXPECTEDENDTAG= 14051 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNCLOSEDTAG     = 14052 :: SystemErrorCode

pattern ERROR_SXS_XML_E_DUPLICATEATTRIBUTE= 14053 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MULTIPLEROOTS   = 14054 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALIDATROOTLEVEL= 14055 :: SystemErrorCode

pattern ERROR_SXS_XML_E_BADXMLDECL      = 14056 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MISSINGROOT     = 14057 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNEXPECTEDEOF   = 14058 :: SystemErrorCode

pattern ERROR_SXS_XML_E_BADPEREFINSUBSET= 14059 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNCLOSEDSTARTTAG= 14060 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNCLOSEDENDTAG  = 14061 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNCLOSEDSTRING  = 14062 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNCLOSEDCOMMENT = 14063 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNCLOSEDDECL    = 14064 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNCLOSEDCDATA   = 14065 :: SystemErrorCode

pattern ERROR_SXS_XML_E_RESERVEDNAMESPACE= 14066 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALIDENCODING = 14067 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALIDSWITCH   = 14068 :: SystemErrorCode

pattern ERROR_SXS_XML_E_BADXMLCASE      = 14069 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALID_STANDALONE= 14070 :: SystemErrorCode

pattern ERROR_SXS_XML_E_UNEXPECTED_STANDALONE= 14071 :: SystemErrorCode

pattern ERROR_SXS_XML_E_INVALID_VERSION = 14072 :: SystemErrorCode

pattern ERROR_SXS_XML_E_MISSINGEQUALS   = 14073 :: SystemErrorCode

pattern ERROR_SXS_PROTECTION_RECOVERY_FAILED= 14074 :: SystemErrorCode

pattern ERROR_SXS_PROTECTION_PUBLIC_KEY_TOO_SHORT= 14075 :: SystemErrorCode

pattern ERROR_SXS_PROTECTION_CATALOG_NOT_VALID= 14076 :: SystemErrorCode

pattern ERROR_SXS_UNTRANSLATABLE_HRESULT= 14077 :: SystemErrorCode

pattern ERROR_SXS_PROTECTION_CATALOG_FILE_MISSING= 14078 :: SystemErrorCode

pattern ERROR_SXS_MISSING_ASSEMBLY_IDENTITY_ATTRIBUTE= 14079 :: SystemErrorCode

pattern ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE_NAME= 14080 :: SystemErrorCode

pattern ERROR_SXS_ASSEMBLY_MISSING      = 14081 :: SystemErrorCode

pattern ERROR_SXS_CORRUPT_ACTIVATION_STACK= 14082 :: SystemErrorCode

pattern ERROR_SXS_CORRUPTION            = 14083 :: SystemErrorCode

pattern ERROR_SXS_EARLY_DEACTIVATION    = 14084 :: SystemErrorCode

pattern ERROR_SXS_INVALID_DEACTIVATION  = 14085 :: SystemErrorCode

pattern ERROR_SXS_MULTIPLE_DEACTIVATION = 14086 :: SystemErrorCode

pattern ERROR_SXS_PROCESS_TERMINATION_REQUESTED= 14087 :: SystemErrorCode

pattern ERROR_SXS_RELEASE_ACTIVATION_CONTEXT= 14088 :: SystemErrorCode

pattern ERROR_SXS_SYSTEM_DEFAULT_ACTIVATION_CONTEXT_EMPTY= 14089 :: SystemErrorCode

pattern ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_VALUE= 14090 :: SystemErrorCode

pattern ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_NAME= 14091 :: SystemErrorCode

pattern ERROR_SXS_IDENTITY_DUPLICATE_ATTRIBUTE= 14092 :: SystemErrorCode

pattern ERROR_SXS_IDENTITY_PARSE_ERROR  = 14093 :: SystemErrorCode

pattern ERROR_MALFORMED_SUBSTITUTION_STRING= 14094 :: SystemErrorCode

pattern ERROR_SXS_INCORRECT_PUBLIC_KEY_TOKEN= 14095 :: SystemErrorCode

pattern ERROR_UNMAPPED_SUBSTITUTION_STRING= 14096 :: SystemErrorCode

pattern ERROR_SXS_ASSEMBLY_NOT_LOCKED   = 14097 :: SystemErrorCode

pattern ERROR_SXS_COMPONENT_STORE_CORRUPT= 14098 :: SystemErrorCode

pattern ERROR_ADVANCED_INSTALLER_FAILED = 14099 :: SystemErrorCode

pattern ERROR_XML_ENCODING_MISMATCH     = 14100 :: SystemErrorCode

pattern ERROR_SXS_MANIFEST_IDENTITY_SAME_BUT_CONTENTS_DIFFERENT= 14101 :: SystemErrorCode

pattern ERROR_SXS_IDENTITIES_DIFFERENT  = 14102 :: SystemErrorCode

pattern ERROR_SXS_ASSEMBLY_IS_NOT_A_DEPLOYMENT= 14103 :: SystemErrorCode

pattern ERROR_SXS_FILE_NOT_PART_OF_ASSEMBLY= 14104 :: SystemErrorCode

pattern ERROR_SXS_MANIFEST_TOO_BIG      = 14105 :: SystemErrorCode

pattern ERROR_SXS_SETTING_NOT_REGISTERED= 14106 :: SystemErrorCode

pattern ERROR_SXS_TRANSACTION_CLOSURE_INCOMPLETE= 14107 :: SystemErrorCode

pattern ERROR_SMI_PRIMITIVE_INSTALLER_FAILED= 14108 :: SystemErrorCode

pattern ERROR_GENERIC_COMMAND_FAILED    = 14109 :: SystemErrorCode

pattern ERROR_SXS_FILE_HASH_MISSING     = 14110 :: SystemErrorCode

pattern ERROR_EVT_INVALID_CHANNEL_PATH  = 15000 :: SystemErrorCode

pattern ERROR_EVT_INVALID_QUERY         = 15001 :: SystemErrorCode

pattern ERROR_EVT_PUBLISHER_METADATA_NOT_FOUND= 15002 :: SystemErrorCode

pattern ERROR_EVT_EVENT_TEMPLATE_NOT_FOUND= 15003 :: SystemErrorCode

pattern ERROR_EVT_INVALID_PUBLISHER_NAME= 15004 :: SystemErrorCode

pattern ERROR_EVT_INVALID_EVENT_DATA    = 15005 :: SystemErrorCode

pattern ERROR_EVT_CHANNEL_NOT_FOUND     = 15007 :: SystemErrorCode

pattern ERROR_EVT_MALFORMED_XML_TEXT    = 15008 :: SystemErrorCode

pattern ERROR_EVT_SUBSCRIPTION_TO_DIRECT_CHANNEL= 15009 :: SystemErrorCode

pattern ERROR_EVT_CONFIGURATION_ERROR   = 15010 :: SystemErrorCode

pattern ERROR_EVT_QUERY_RESULT_STALE    = 15011 :: SystemErrorCode

pattern ERROR_EVT_QUERY_RESULT_INVALID_POSITION= 15012 :: SystemErrorCode

pattern ERROR_EVT_NON_VALIDATING_MSXML  = 15013 :: SystemErrorCode

pattern ERROR_EVT_FILTER_ALREADYSCOPED  = 15014 :: SystemErrorCode

pattern ERROR_EVT_FILTER_NOTELTSET      = 15015 :: SystemErrorCode

pattern ERROR_EVT_FILTER_INVARG         = 15016 :: SystemErrorCode

pattern ERROR_EVT_FILTER_INVTEST        = 15017 :: SystemErrorCode

pattern ERROR_EVT_FILTER_INVTYPE        = 15018 :: SystemErrorCode

pattern ERROR_EVT_FILTER_PARSEERR       = 15019 :: SystemErrorCode

pattern ERROR_EVT_FILTER_UNSUPPORTEDOP  = 15020 :: SystemErrorCode

pattern ERROR_EVT_FILTER_UNEXPECTEDTOKEN= 15021 :: SystemErrorCode

pattern ERROR_EVT_INVALID_OPERATION_OVER_ENABLED_DIRECT_CHANNEL= 15022 :: SystemErrorCode

pattern ERROR_EVT_INVALID_CHANNEL_PROPERTY_VALUE= 15023 :: SystemErrorCode

pattern ERROR_EVT_INVALID_PUBLISHER_PROPERTY_VALUE= 15024 :: SystemErrorCode

pattern ERROR_EVT_CHANNEL_CANNOT_ACTIVATE= 15025 :: SystemErrorCode

pattern ERROR_EVT_FILTER_TOO_COMPLEX    = 15026 :: SystemErrorCode

pattern ERROR_EVT_MESSAGE_NOT_FOUND     = 15027 :: SystemErrorCode

pattern ERROR_EVT_MESSAGE_ID_NOT_FOUND  = 15028 :: SystemErrorCode

pattern ERROR_EVT_UNRESOLVED_VALUE_INSERT= 15029 :: SystemErrorCode

pattern ERROR_EVT_UNRESOLVED_PARAMETER_INSERT= 15030 :: SystemErrorCode

pattern ERROR_EVT_MAX_INSERTS_REACHED   = 15031 :: SystemErrorCode

pattern ERROR_EVT_EVENT_DEFINITION_NOT_FOUND= 15032 :: SystemErrorCode

pattern ERROR_EVT_MESSAGE_LOCALE_NOT_FOUND= 15033 :: SystemErrorCode

pattern ERROR_EVT_VERSION_TOO_OLD       = 15034 :: SystemErrorCode

pattern ERROR_EVT_VERSION_TOO_NEW       = 15035 :: SystemErrorCode

pattern ERROR_EVT_CANNOT_OPEN_CHANNEL_OF_QUERY= 15036 :: SystemErrorCode

pattern ERROR_EVT_PUBLISHER_DISABLED    = 15037 :: SystemErrorCode

pattern ERROR_EVT_FILTER_OUT_OF_RANGE   = 15038 :: SystemErrorCode

pattern ERROR_EC_SUBSCRIPTION_CANNOT_ACTIVATE= 15080 :: SystemErrorCode

pattern ERROR_EC_LOG_DISABLED           = 15081 :: SystemErrorCode

pattern ERROR_EC_CIRCULAR_FORWARDING    = 15082 :: SystemErrorCode

pattern ERROR_EC_CREDSTORE_FULL         = 15083 :: SystemErrorCode

pattern ERROR_EC_CRED_NOT_FOUND         = 15084 :: SystemErrorCode

pattern ERROR_EC_NO_ACTIVE_CHANNEL      = 15085 :: SystemErrorCode

pattern ERROR_MUI_FILE_NOT_FOUND        = 15100 :: SystemErrorCode

pattern ERROR_MUI_INVALID_FILE          = 15101 :: SystemErrorCode

pattern ERROR_MUI_INVALID_RC_CONFIG     = 15102 :: SystemErrorCode

pattern ERROR_MUI_INVALID_LOCALE_NAME   = 15103 :: SystemErrorCode

pattern ERROR_MUI_INVALID_ULTIMATEFALLBACK_NAME= 15104 :: SystemErrorCode

pattern ERROR_MUI_FILE_NOT_LOADED       = 15105 :: SystemErrorCode

pattern ERROR_RESOURCE_ENUM_USER_STOP   = 15106 :: SystemErrorCode

pattern ERROR_MUI_INTLSETTINGS_UILANG_NOT_INSTALLED= 15107 :: SystemErrorCode

pattern ERROR_MUI_INTLSETTINGS_INVALID_LOCALE_NAME= 15108 :: SystemErrorCode

pattern ERROR_MRM_RUNTIME_NO_DEFAULT_OR_NEUTRAL_RESOURCE= 15110 :: SystemErrorCode

pattern ERROR_MRM_INVALID_PRICONFIG     = 15111 :: SystemErrorCode

pattern ERROR_MRM_INVALID_FILE_TYPE     = 15112 :: SystemErrorCode

pattern ERROR_MRM_UNKNOWN_QUALIFIER     = 15113 :: SystemErrorCode

pattern ERROR_MRM_INVALID_QUALIFIER_VALUE= 15114 :: SystemErrorCode

pattern ERROR_MRM_NO_CANDIDATE          = 15115 :: SystemErrorCode

pattern ERROR_MRM_NO_MATCH_OR_DEFAULT_CANDIDATE= 15116 :: SystemErrorCode

pattern ERROR_MRM_RESOURCE_TYPE_MISMATCH= 15117 :: SystemErrorCode

pattern ERROR_MRM_DUPLICATE_MAP_NAME    = 15118 :: SystemErrorCode

pattern ERROR_MRM_DUPLICATE_ENTRY       = 15119 :: SystemErrorCode

pattern ERROR_MRM_INVALID_RESOURCE_IDENTIFIER= 15120 :: SystemErrorCode

pattern ERROR_MRM_FILEPATH_TOO_LONG     = 15121 :: SystemErrorCode

pattern ERROR_MRM_UNSUPPORTED_DIRECTORY_TYPE= 15122 :: SystemErrorCode

pattern ERROR_MRM_INVALID_PRI_FILE      = 15126 :: SystemErrorCode

pattern ERROR_MRM_NAMED_RESOURCE_NOT_FOUND= 15127 :: SystemErrorCode

pattern ERROR_MRM_MAP_NOT_FOUND         = 15135 :: SystemErrorCode

pattern ERROR_MRM_UNSUPPORTED_PROFILE_TYPE= 15136 :: SystemErrorCode

pattern ERROR_MRM_INVALID_QUALIFIER_OPERATOR= 15137 :: SystemErrorCode

pattern ERROR_MRM_INDETERMINATE_QUALIFIER_VALUE= 15138 :: SystemErrorCode

pattern ERROR_MRM_AUTOMERGE_ENABLED     = 15139 :: SystemErrorCode

pattern ERROR_MRM_TOO_MANY_RESOURCES    = 15140 :: SystemErrorCode

pattern ERROR_MRM_UNSUPPORTED_FILE_TYPE_FOR_MERGE= 15141 :: SystemErrorCode

pattern ERROR_MRM_UNSUPPORTED_FILE_TYPE_FOR_LOAD_UNLOAD_PRI_FILE= 15142 :: SystemErrorCode

pattern ERROR_MRM_NO_CURRENT_VIEW_ON_THREAD= 15143 :: SystemErrorCode

pattern ERROR_DIFFERENT_PROFILE_RESOURCE_MANAGER_EXIST= 15144 :: SystemErrorCode

pattern ERROR_OPERATION_NOT_ALLOWED_FROM_SYSTEM_COMPONENT= 15145 :: SystemErrorCode

pattern ERROR_MRM_DIRECT_REF_TO_NON_DEFAULT_RESOURCE= 15146 :: SystemErrorCode

pattern ERROR_MRM_GENERATION_COUNT_MISMATCH= 15147 :: SystemErrorCode

pattern ERROR_MCA_INVALID_CAPABILITIES_STRING= 15200 :: SystemErrorCode

pattern ERROR_MCA_INVALID_VCP_VERSION   = 15201 :: SystemErrorCode

pattern ERROR_MCA_MONITOR_VIOLATES_MCCS_SPECIFICATION= 15202 :: SystemErrorCode

pattern ERROR_MCA_MCCS_VERSION_MISMATCH = 15203 :: SystemErrorCode

pattern ERROR_MCA_UNSUPPORTED_MCCS_VERSION= 15204 :: SystemErrorCode

pattern ERROR_MCA_INTERNAL_ERROR        = 15205 :: SystemErrorCode

pattern ERROR_MCA_INVALID_TECHNOLOGY_TYPE_RETURNED= 15206 :: SystemErrorCode

pattern ERROR_MCA_UNSUPPORTED_COLOR_TEMPERATURE= 15207 :: SystemErrorCode

pattern ERROR_AMBIGUOUS_SYSTEM_DEVICE   = 15250 :: SystemErrorCode

pattern ERROR_SYSTEM_DEVICE_NOT_FOUND   = 15299 :: SystemErrorCode

pattern ERROR_HASH_NOT_SUPPORTED        = 15300 :: SystemErrorCode

pattern ERROR_HASH_NOT_PRESENT          = 15301 :: SystemErrorCode

pattern ERROR_SECONDARY_IC_PROVIDER_NOT_REGISTERED= 15321 :: SystemErrorCode

pattern ERROR_GPIO_CLIENT_INFORMATION_INVALID= 15322 :: SystemErrorCode

pattern ERROR_GPIO_VERSION_NOT_SUPPORTED= 15323 :: SystemErrorCode

pattern ERROR_GPIO_INVALID_REGISTRATION_PACKET= 15324 :: SystemErrorCode

pattern ERROR_GPIO_OPERATION_DENIED     = 15325 :: SystemErrorCode

pattern ERROR_GPIO_INCOMPATIBLE_CONNECT_MODE= 15326 :: SystemErrorCode

pattern ERROR_GPIO_INTERRUPT_ALREADY_UNMASKED= 15327 :: SystemErrorCode

pattern ERROR_CANNOT_SWITCH_RUNLEVEL    = 15400 :: SystemErrorCode

pattern ERROR_INVALID_RUNLEVEL_SETTING  = 15401 :: SystemErrorCode

pattern ERROR_RUNLEVEL_SWITCH_TIMEOUT   = 15402 :: SystemErrorCode

pattern ERROR_RUNLEVEL_SWITCH_AGENT_TIMEOUT= 15403 :: SystemErrorCode

pattern ERROR_RUNLEVEL_SWITCH_IN_PROGRESS= 15404 :: SystemErrorCode

pattern ERROR_SERVICES_FAILED_AUTOSTART = 15405 :: SystemErrorCode

pattern ERROR_COM_TASK_STOP_PENDING     = 15501 :: SystemErrorCode

pattern ERROR_INSTALL_OPEN_PACKAGE_FAILED= 15600 :: SystemErrorCode

pattern ERROR_INSTALL_PACKAGE_NOT_FOUND = 15601 :: SystemErrorCode

pattern ERROR_INSTALL_INVALID_PACKAGE   = 15602 :: SystemErrorCode

pattern ERROR_INSTALL_RESOLVE_DEPENDENCY_FAILED= 15603 :: SystemErrorCode

pattern ERROR_INSTALL_OUT_OF_DISK_SPACE = 15604 :: SystemErrorCode

pattern ERROR_INSTALL_NETWORK_FAILURE   = 15605 :: SystemErrorCode

pattern ERROR_INSTALL_REGISTRATION_FAILURE= 15606 :: SystemErrorCode

pattern ERROR_INSTALL_DEREGISTRATION_FAILURE= 15607 :: SystemErrorCode

pattern ERROR_INSTALL_CANCEL            = 15608 :: SystemErrorCode

pattern ERROR_INSTALL_FAILED            = 15609 :: SystemErrorCode

pattern ERROR_REMOVE_FAILED             = 15610 :: SystemErrorCode

pattern ERROR_PACKAGE_ALREADY_EXISTS    = 15611 :: SystemErrorCode

pattern ERROR_NEEDS_REMEDIATION         = 15612 :: SystemErrorCode

pattern ERROR_INSTALL_PREREQUISITE_FAILED= 15613 :: SystemErrorCode

pattern ERROR_PACKAGE_REPOSITORY_CORRUPTED= 15614 :: SystemErrorCode

pattern ERROR_INSTALL_POLICY_FAILURE    = 15615 :: SystemErrorCode

pattern ERROR_PACKAGE_UPDATING          = 15616 :: SystemErrorCode

pattern ERROR_DEPLOYMENT_BLOCKED_BY_POLICY= 15617 :: SystemErrorCode

pattern ERROR_PACKAGES_IN_USE           = 15618 :: SystemErrorCode

pattern ERROR_RECOVERY_FILE_CORRUPT     = 15619 :: SystemErrorCode

pattern ERROR_INVALID_STAGED_SIGNATURE  = 15620 :: SystemErrorCode

pattern ERROR_DELETING_EXISTING_APPLICATIONDATA_STORE_FAILED= 15621 :: SystemErrorCode

pattern ERROR_INSTALL_PACKAGE_DOWNGRADE = 15622 :: SystemErrorCode

pattern ERROR_SYSTEM_NEEDS_REMEDIATION  = 15623 :: SystemErrorCode

pattern ERROR_APPX_INTEGRITY_FAILURE_CLR_NGEN= 15624 :: SystemErrorCode

pattern ERROR_RESILIENCY_FILE_CORRUPT   = 15625 :: SystemErrorCode

pattern ERROR_INSTALL_FIREWALL_SERVICE_NOT_RUNNING= 15626 :: SystemErrorCode

pattern ERROR_STATE_LOAD_STORE_FAILED   = 15800 :: SystemErrorCode

pattern ERROR_STATE_GET_VERSION_FAILED  = 15801 :: SystemErrorCode

pattern ERROR_STATE_SET_VERSION_FAILED  = 15802 :: SystemErrorCode

pattern ERROR_STATE_STRUCTURED_RESET_FAILED= 15803 :: SystemErrorCode

pattern ERROR_STATE_OPEN_CONTAINER_FAILED= 15804 :: SystemErrorCode

pattern ERROR_STATE_CREATE_CONTAINER_FAILED= 15805 :: SystemErrorCode

pattern ERROR_STATE_DELETE_CONTAINER_FAILED= 15806 :: SystemErrorCode

pattern ERROR_STATE_READ_SETTING_FAILED = 15807 :: SystemErrorCode

pattern ERROR_STATE_WRITE_SETTING_FAILED= 15808 :: SystemErrorCode

pattern ERROR_STATE_DELETE_SETTING_FAILED= 15809 :: SystemErrorCode

pattern ERROR_STATE_QUERY_SETTING_FAILED= 15810 :: SystemErrorCode

pattern ERROR_STATE_READ_COMPOSITE_SETTING_FAILED= 15811 :: SystemErrorCode

pattern ERROR_STATE_WRITE_COMPOSITE_SETTING_FAILED= 15812 :: SystemErrorCode

pattern ERROR_STATE_ENUMERATE_CONTAINER_FAILED= 15813 :: SystemErrorCode

pattern ERROR_STATE_ENUMERATE_SETTINGS_FAILED= 15814 :: SystemErrorCode

pattern ERROR_STATE_COMPOSITE_SETTING_VALUE_SIZE_LIMIT_EXCEEDED= 15815 :: SystemErrorCode

pattern ERROR_STATE_SETTING_VALUE_SIZE_LIMIT_EXCEEDED= 15816 :: SystemErrorCode

pattern ERROR_STATE_SETTING_NAME_SIZE_LIMIT_EXCEEDED= 15817 :: SystemErrorCode

pattern ERROR_STATE_CONTAINER_NAME_SIZE_LIMIT_EXCEEDED= 15818 :: SystemErrorCode

pattern ERROR_API_UNAVAILABLE           = 15841 :: SystemErrorCode
