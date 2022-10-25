!> IO ファイル名モジュール
module mod_gedatsu_io_file_name
  use mod_gedatsu_prm
  use mod_gedatsu_util
  implicit none

contains

  !> @ingroup group_io
  !> 並列計算用書き出しファイル名の取得
  function gedatsu_get_output_file_name(dirname, fname, domain_id)
    implicit none
    !> [out] 戻り値
    character(gedatsu_charlen) :: gedatsu_get_output_file_name
    !> [in] 出力ディレクトリ名
    character(gedatsu_charlen) :: dirname
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] 領域番号
    integer(gint) :: domain_id
    character(gedatsu_charlen) :: cid

    write(cid,"(i0)") domain_id
    gedatsu_get_output_file_name = trim(dirname)//"/"//trim(fname)//"."//trim(cid)
  end function gedatsu_get_output_file_name
end module mod_gedatsu_io_file_name
