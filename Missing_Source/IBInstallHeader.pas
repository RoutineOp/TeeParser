unit IBInstallHeader;

interface

type
  Tisc_install_clear_options=Pointer;
  Tisc_install_execute=Pointer;
  Tisc_install_get_info=Pointer;
  Tisc_install_get_message=Pointer;
  Tisc_install_load_external_text=Pointer;
  Tisc_install_precheck=Pointer;
  Tisc_install_set_option=Pointer;
  Tisc_uninstall_execute=Pointer;
  Tisc_uninstall_precheck=Pointer;
  Tisc_install_unset_option=Pointer;

  FP_STATUS=Pointer;
  FP_ERROR=Pointer;
  
  OPTIONS_HANDLE=Integer;
  POPTIONS_HANDLE=Pointer;
  MSG_NO=Integer;
  OPT=Pointer;

const
  IB_INSTALL_DLL='';

var
 isc_install_clear_options_stub:Pointer;
 isc_install_execute_stub:Pointer;
 isc_install_get_info_stub:Pointer;
 isc_install_get_message_stub:Pointer;
 isc_install_load_external_text_stub:Pointer;
 isc_install_precheck_stub:Pointer;
 isc_install_set_option_stub:Pointer;
 isc_uninstall_execute_stub:Pointer;
 isc_uninstall_precheck_stub:Pointer;
 isc_install_unset_option_stub:Pointer;

implementation

end.
