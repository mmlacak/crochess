#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# from scene import Scene
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
# from scene_nineteen import SceneNineteenMixin
# from scene_hemeras_dawn import SceneHemerasDawnMixin
# from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
# from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin


# class SceneMix(SceneCroatianTiesMixin, \
#                SceneMayanAscendancyMixin, \
#                SceneAgeOfAquariusMixin, \
#                SceneMirandasVeilMixin, \
#                SceneNineteenMixin, \
#                SceneHemerasDawnMixin, \
#                SceneTamoanchanRevisitedMixin, \
#                SceneConquestOfTlalocanMixin):

class SceneMix(SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin, \
               SceneAgeOfAquariusMixin, \
               SceneMirandasVeilMixin):

    def _get_recent_scene_method_names(self):
        return  [
                    'scn_mv_01_move_wave_init', \
                    'scn_mv_02_move_wave_activated', \
                    'scn_mv_03_move_wave_finished', \
                    'scn_mv_04_cascading_rook', \
                    'scn_mv_05_cascading_wave_1', \

                    'scn_mv_06_cascading_wave_2', \
                    'scn_mv_07_cascading_rook_2nd_time', \
                    'scn_mv_08_cascading_wave_1_2nd_time', \
                    'scn_mv_09_cascading_queen', \
                    'scn_mv_10_cascading_wave_2_2nd_time', \

                    'scn_mv_11_cascading_wave_1_3rd_time', \
                    'scn_mv_12_cascading_end', \
                    'scn_mv_13_casc_oppo_light_queen', \
                    'scn_mv_14_casc_oppo_light_wave', \
                    'scn_mv_15_casc_oppo_dark_wave', \

                    'scn_mv_16_casc_oppo_dark_queen', \
                    'scn_mv_17_casc_oppo_end', \
                    'scn_mv_18_activating_rush_pawn_init', \
                    'scn_mv_19_activating_rush_pawn_end', \
                    'scn_mv_20_wave_activation_by_step_pawn', \

                    'scn_mv_21_wave_activated_by_step_pawn', \
                    'scn_mv_22_wave_activation_by_capture_pawn', \
                    'scn_mv_23_wave_activated_by_capture_pawn', \
                    'scn_mv_24_wave_activation_by_unicorn', \
                    'scn_mv_25_wave_activated_by_unicorn', \

                    'scn_mv_26_wave_off_board', \
                ]

    def _get_all_scene_method_names(self):
        return [ n for n in dir(self) if n.startswith('scn_') ] # or n.startswith('move_')

    def _get_attributes(self, names):
        return [ getattr(self, a) for a in names ]

    def get_recent_scene_methods(self):
        return self._get_attributes( self._get_recent_scene_method_names() )

    def get_all_scene_methods(self):
        return self._get_attributes( self._get_all_scene_method_names() )
